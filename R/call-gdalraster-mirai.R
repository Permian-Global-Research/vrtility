#' @importFrom methods new
#' @importFrom gdalraster GDALRaster
call_gdalraster_mirai <- function(
  x,
  outfile = fs::file_temp(ext = "tif"),
  nsplits = 2L,
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = FALSE,
  return_internals = FALSE
) {
  orig_config <- set_gdal_config(c(config_options))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  vrt_template <- vrt_save(x)
  ds <- new(gdalraster::GDALRaster, vrt_template)
  xs <- ds$getRasterXSize()
  ys <- ds$getRasterYSize()
  nodataval <- ds$getNoDataValue(1)
  blksize <- ds$getBlockSize(1)
  nbands <- ds$getRasterCount()
  blocks_df <- as.data.frame(get_tiles(xs, ys, blksize[1], blksize[2])) |>
    aggregate_blocks(nsplits) |>
    merge(data.frame(band_n = seq_len(nbands)))

  blocks <- split(blocks_df, seq_len(nrow(blocks_df)))
  ds$close()

  # Initialize output raster
  suppressMessages(gdalraster::rasterFromRaster(
    vrt_template,
    outfile,
    fmt = "GTiff",
    init = nodataval,
    options = creation_options
  ))

  # inital data open in advance of promise eval.
  ds <- new(gdalraster::GDALRaster, outfile, read_only = FALSE)
  on.exit(ds$close(), add = TRUE)

  # Create mirai map with promises
  jobs <- mirai::mirai_map(
    blocks,
    function(b) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- unlist(b)

      ds <- methods::new(gdalraster::GDALRaster, vrt_file)
      on.exit(ds$close())
      # Read and combine bands
      band_data <- vrtility::compute_with_py_env(
        ds$read(
          band = block_params["band_n"],
          xoff = block_params["nXOff"],
          yoff = block_params["nYOff"],
          xsize = block_params["nXSize"],
          ysize = block_params["nYSize"],
          out_xsize = block_params["nXSize"],
          out_ysize = block_params["nYSize"]
        )
      )

      list(
        data = unlist(band_data),
        block = c(
          band_n = b["band_n"],
          xoff = b["nXOff"],
          yoff = b["nYOff"],
          xsize = b["nXSize"],
          ysize = b["nYSize"]
        )
      )
    },
    vrt_file = vrt_template,
    nbands = nbands,
    .promise = list(
      # On success
      function(result) {
        if (!ds$isOpen()) {
          ds <- new(gdalraster::GDALRaster, outfile, read_only = FALSE)
        }

        ds$write(
          band = result$block[[1]],
          xoff = result$block[[2]],
          yoff = result$block[[3]],
          xsize = result$block[[4]],
          ysize = result$block[[5]],
          rasterData = result$data
        )
      },
      # On error
      function(error) {
        cli::cli_abort(
          c(
            "x" = "Failed to process block...",
            " " = "{conditionMessage(error)}"
          )
        )
      }
    )
  )

  if (!quiet) {
    result <- jobs[.progress]
  } else {
    result <- jobs[]
  }

  if (return_internals) {
    return(
      list(
        outfile = outfile,
        internals = result
      )
    )
  }

  return(outfile)
}
