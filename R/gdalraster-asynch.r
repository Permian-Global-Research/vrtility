async_gdalreader <- function(blocks, vrt_file, ds) {
  # Create mirai map with promises
  jobs <- purrr::pmap(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      mirai::mirai(
        {
          inds <- methods::new(gdalraster::GDALRaster, vrt_file)
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
          return(list(
            band_data = band_data,
            block_params = block_params
          ))
        },
        vrt_file = vrt_file,
        block_params = block_params,
        compute_with_py_env = compute_with_py_env
      )
    }
  )

  while (mirai::unresolved(jobs)) {
    purrr::walk(jobs, function(j) {
      if (!inherits(j, "unresolvedValue")) {
        j <- j[]
        ds$write(
          band = j$block_params[["band_n"]],
          xoff = j$block_params[["nXOff"]],
          yoff = j$block_params[["nYOff"]],
          xsize = j$block_params[["nXSize"]],
          ysize = j$block_params[["nYSize"]],
          rasterData = j$band_data
        )
      }
    })
  }
}
