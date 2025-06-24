async_gdalreader <- function(blocks, vrt_file, ds) {
  # Create mirai map with promises
  jobs <- mirai::mirai_map(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      inds <- methods::new(
        gdalraster::GDALRaster,
        vrt_file
      )
      on.exit(inds$close())
      # # Read and combine bands
      band_data <- compute_with_py_env(
        inds$read(
          band = block_params[["band_n"]],
          xoff = block_params[["nXOff"]],
          yoff = block_params[["nYOff"]],
          xsize = block_params[["nXSize"]],
          ysize = block_params[["nYSize"]],
          out_xsize = block_params[["nXSize"]],
          out_ysize = block_params[["nYSize"]]
        )
      )

      bscale <- inds$getScale(block_params[["band_n"]])
      if (!is.na(bscale)) {
        band_data <- band_data * bscale
      }

      return(band_data)
    },
    vrt_file = vrt_file,
    block_params = blocks,
    compute_with_py_env = compute_with_py_env
  )

  resolved <- logical(length(jobs))

  while (mirai::unresolved(jobs)) {
    for (i in seq_along(jobs)) {
      if (inherits(jobs[[i]]$data, "unresolvedValue")) {
        next
      }

      if (resolved[[i]]) {
        next
      }

      j <- jobs[[i]][]

      if (inherits(j, "character")) {
        cli::cli_abort(
          "mirai GDAL read error:  {j}",
          class = "mirai_gdal_read_error"
        )
      }

      bps <- blocks[i, ]

      ds$write(
        band = bps[["band_n"]],
        xoff = bps[["nXOff"]],
        yoff = bps[["nYOff"]],
        xsize = bps[["nXSize"]],
        ysize = bps[["nYSize"]],
        rasterData = j
      )
      # If the job is resolved, mark it as such
      resolved[[i]] <- TRUE
    }
  }
}
