#' @importFrom methods new
#' @importFrom gdalraster GDALRaster
call_gdalraster_mirai <- function(
  src_files,
  outfile,
  resampling,
  cl_arg,
  config_options,
  nsplits = 2L,
  quiet = TRUE
) {
  # init checks and setup.
  v_assert_true(fs::file_exists(src_files), "src_files")

  orig_config <- set_gdal_config(c(
    config_options,
    GDAL_RASTERIO_RESAMPLING = resampling
  ))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # read the input vrt.
  ds_main <- new(GDALRaster, src_files, read_only = TRUE)
  on.exit(ds_main$close(), add = TRUE)

  # get required metadata.
  nbands <- ds_main$getRasterCount()

  # set up mirai processes (daemons) for parallel processing.
  o_mirari_cons <- set_outer_daemons(nbands)
  if (o_mirari_cons == 0) on.exit(mirai::daemons(0), add = TRUE)

  # main function to run chunked/parallel processing.
  band_files <- map_bands_and_chunks(
    nbands = nbands,
    src_files = src_files,
    nsplits = nsplits,
    save_dir = getOption("vrt.cache"),
    config_options = config_options
  )

  # combining all the outputs of the parallel processing.
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  gdalraster::buildVRT(
    tf,
    unlist(band_files),
    cl_arg = c("-separate"),
    quiet = TRUE
  )

  bnames <- purrr::map_chr(
    seq_len(nbands),
    ~ ds_main$getDescription(.x)
  )

  tf_named <- set_vrt_descriptions(
    x = tf,
    descriptions = bnames,
    as_file = TRUE
  )

  gdalraster::translate(
    tf_named,
    outfile,
    quiet = TRUE,
    cl_arg = cl_arg
  )

  #

  return(outfile)
}

#' For mapping over bands and chunks of a raster for parallel
#' processing.
#' @param nbands the number of bands in the raster
#' @param src_files the source files
#' @param nsplits the number of splits
#' @param save_dir the directory to save the temporary files
#' @param config_options the configuration options
#' @return a list of file paths
#' @noRd
#' @keywords internal
#'
map_bands_and_chunks <- function(
  nbands,
  src_files,
  nsplits,
  save_dir,
  config_options
) {
  purrr::map(
    seq_len(nbands),
    carrier::crate(
      function(b) {
        vrtility::set_gdal_config(config_options)
        vrtility::compute_with_py_env({
          ds_in <- methods::new(
            gdalraster::GDALRaster,
            src_files,
            read_only = TRUE
          )
          on.exit(ds_in$close(), add = TRUE)
          nrows <- ds_in$getRasterYSize()

          row_splits <- seq(0, nrows - 1, length.out = nsplits + 1)
          row_ranges <- data.frame(
            start = row_splits[-length(row_splits)],
            end = row_splits[-1] - 1
          )
          row_ranges$end[nrow(row_ranges)] <- nrows - 1

          with(mirai::daemons(nsplits), {
            inner_chunks <- purrr::pmap_chr(
              row_ranges,
              carrier::crate(
                function(start, end) {
                  vrtility::set_gdal_config(config_options)
                  vrtility::compute_with_py_env({
                    # read source
                    inner_ds_in <- methods::new(
                      gdalraster::GDALRaster,
                      in_files,
                      read_only = TRUE
                    )
                    on.exit(inner_ds_in$close(), add = TRUE)
                    # get key properties
                    ncols <- inner_ds_in$getRasterXSize()
                    nodata_val <- inner_ds_in$getNoDataValue(b)
                    # build output
                    inner_ds_out_src <- gdalraster::rasterFromRaster(
                      in_files,
                      fs::file_temp(
                        tmp_dir = save_dir,
                        ext = "tif"
                      ),
                      fmt = "GTiff",
                      nbands = 1,
                      options = c(
                        "COMPRESS=ZSTD", # Good balance of compression/speed
                        "ZSTD_LEVEL=1", # maybe should be higher?
                        "PREDICTOR=2", # Helps with continuous data
                        "TILED=YES", # Enable tiling for better read performance
                        glue::glue(
                          "BLOCKXSIZE={inner_ds_in$getBlockSize(b)[1]}"
                        ),
                        glue::glue(
                          "BLOCKYSIZE={inner_ds_in$getBlockSize(b)[2]}"
                        ),
                        "BIGTIFF=IF_NEEDED" # Automatic large file handling
                      ),
                      init = nodata_val
                    )
                    inner_ds_out <- methods::new(
                      gdalraster::GDALRaster,
                      inner_ds_out_src,
                      read_only = FALSE
                    )

                    on.exit(inner_ds_out$close(), add = TRUE)

                    # read the "block" just rows for now - should we optimize?
                    inner_r_dat <- inner_ds_in$read(
                      band = b,
                      xoff = 0,
                      yoff = start,
                      xsize = ncols,
                      ysize = end - start + 1,
                      out_xsize = ncols,
                      out_ysize = end - start + 1
                    )
                    # Explicitly set the value of NA to nodata before writing.
                    inner_r_dat[is.na(inner_r_dat)] <- nodata_val
                    # write the block
                    inner_ds_out$write(
                      band = 1,
                      xoff = 0,
                      yoff = start,
                      xsize = ncols,
                      ysize = end - start + 1,
                      rasterData = inner_r_dat
                    )

                    return(inner_ds_out_src)
                  })
                },
                in_files = src_files,
                b = b,
                save_dir = save_dir,
                config_options = config_options
              ),
              .parallel = TRUE
            )
          })

          tvrt <- fs::file_temp(tmp_dir = save_dir, ext = "vrt")
          gdalraster::buildVRT(
            tvrt,
            inner_chunks,
            quiet = TRUE
          )

          ttiff <- fs::file_temp(tmp_dir = save_dir, ext = "tif")
          gdalraster::translate(
            tvrt,
            ttiff,
            quiet = TRUE,
            cl_arg = c(
              "-co",
              "COMPRESS=ZSTD", # Good balance of compression/speed
              "-co",
              "ZSTD_LEVEL=1", # Fastest compression level
              "-co",
              "PREDICTOR=2", # Helps with continuous data
              "-co",
              "TILED=YES", # Enable tiling for better read performance
              "-co",
              glue::glue(
                "BLOCKXSIZE={ds_in$getBlockSize(b)[1]}"
              ),
              "-co",
              glue::glue(
                "BLOCKYSIZE={ds_in$getBlockSize(b)[2]}"
              ),
              "-co",
              "BIGTIFF=IF_NEEDED" # Automatic large file handling
            )
          )

          return(ttiff)
        })
      },
      src_files = src_files,
      nsplits = nsplits,
      save_dir = save_dir,
      config_options = config_options
    ),
    .parallel = TRUE
  )
}
