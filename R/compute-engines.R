#' A very thin wrapper around the `gdalraster::warp` function
#' @keywords internal
#' @noRd
call_gdal_warp <- function(
  src_files,
  outfile,
  t_srs,
  cl_arg,
  config_options,
  quiet = TRUE
) {
  v_assert_true(fs::file_exists(src_files), "src_files")

  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config))

  gdalraster::warp(
    src_files,
    outfile,
    t_srs = t_srs,
    cl_arg = cl_arg,
    quiet = quiet
  )

  return(outfile)
}


#' Combine multiple warper input arguments
#' @noRd
#' @keywords internal
combine_warp_opts <- function(
  creation_options,
  warp_opts,
  resampling,
  te,
  res = NULL,
  add_args = NULL
) {
  opts_check(warp_opts, "-te")
  opts_check(warp_opts, "-tr")
  opts_check(warp_opts, "-r")

  warp_opts <- c(
    creation_options,
    "-r",
    resampling,
    warp_opts,
    "-te",
    te
  )

  if (!is.null(res)) {
    warp_opts <- c(
      warp_opts,
      "-tr",
      res,
      if ("-tap" %in% warp_opts) NULL else "-tap"
    )
  }

  if (!is.null(add_args)) {
    warp_opts <- c(warp_opts, add_args)
  }

  return(warp_opts)
}

#' A very thin wrapper around the `gdalraster::translate` function
#' @keywords internal
#' @noRd
call_gdal_tanslate <- function(
  src_files,
  outfile,
  cl_arg,
  config_options,
  quiet = TRUE
) {
  v_assert_true(fs::file_exists(src_files), "src_files")

  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config))

  gdalraster::translate(
    src_files,
    outfile,
    cl_arg = cl_arg,
    quiet = quiet
  )

  return(outfile)
}


#' @importFrom methods new
#' @importFrom gdalraster GDALRaster
call_gdalraster_mirai <- function(
  src_files,
  outfile,
  cl_arg,
  config_options,
  nsplits = 2L,
  quiet = TRUE
) {
  v_assert_true(fs::file_exists(src_files), "src_files")
  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config))

  ds_main <- new(GDALRaster, src_files, read_only = TRUE)
  on.exit(ds_main$close())
  nbands <- ds_main$getRasterCount()

  if (mirai::status()$connections == 0) {
    mirai::daemons(nbands)
    on.exit(mirai::daemons(0L))
  } else if (mirai::status()$connections < nbands) {
    cli::cli_warn(
      c(
        "!" = "Active mirai daemons have been detected, but fewer than the number of
      bands.",
        "i" = "No changes were made to this mirai configuartion but this could 
        result in performance issues"
      )
    )
  }

  band_files <- purrr::map(
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
          on.exit(ds_in$close())
          nrows <- ds_in$getRasterYSize()

          row_splits <- seq(0, nrows - 1, length.out = nsplits + 1)
          row_ranges <- data.frame(
            start = row_splits[-length(row_splits)],
            end = row_splits[-1] - 1
          )
          row_ranges$end[nrow(row_ranges)] <- nrows - 1

          mirai::daemons(nsplits)

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
                  on.exit(inner_ds_in$close())
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
                    nbands = 1,
                    dstnodata = nodata_val
                  )
                  inner_ds_out <- methods::new(
                    gdalraster::GDALRaster,
                    inner_ds_out_src,
                    read_only = FALSE
                  )
                  on.exit(inner_ds_out$close())

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
            quiet = TRUE
          )

          return(ttiff)
        })
      },
      src_files = src_files,
      nsplits = nsplits,
      save_dir = getOption("vrt.cache"),
      config_options = config_options
    ),
    .parallel = TRUE
  )

  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  gdalraster::buildVRT(
    tf,
    unlist(band_files),
    cl_arg = c("-separate"),
    quiet = TRUE
  )
  gdalraster::translate(tf, outfile, quiet = TRUE)

  return(outfile)
}
