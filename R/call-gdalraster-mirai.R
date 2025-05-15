#' @importFrom methods new
#' @importFrom gdalraster GDALRaster
call_gdalraster_mirai <- function(
  x,
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = FALSE,
  nsplits = NULL,
  return_internals = FALSE
) {
  orig_config <- set_gdal_config(c(config_options))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  rt <- raster_template_params(x)
  vrt_template <- rt$vrt_template
  rt$xs <- rt$xs
  rt$ys <- rt$ys
  rt$nodataval <- rt$nodataval
  rt$blksize <- rt$blksize
  rt$nbands <- rt$nbands
  rt$scale_vals <- rt$scale_vals
  rt$data_type <- rt$data_type

  if (is.null(nsplits)) {
    nits <- x$n_items
    if (is.null(nits)) {
      nits <- 1
    }
    nsplits <- suggest_n_chunks(rt$ys, rt$xs, rt$nbands, nits)
  }

  blocks_df <- optimise_tiling(
    rt$xs,
    rt$ys,
    rt$blksize,
    nsplits
  ) |>
    merge(data.frame(band_n = seq_len(rt$nbands)))

  # Initialize output raster
  nr <- suppressMessages(gdalraster::rasterFromRaster(
    vrt_template,
    normalizePath(outfile, mustWork = FALSE),
    fmt = "GTiff",
    init = rt$nodataval,
    options = creation_options,
    dtName = rt$data_type
  ))

  ds <- methods::new(gdalraster::GDALRaster, nr, read_only = FALSE)
  on.exit(ds$close(), add = TRUE)
  purrr::iwalk(x$assets, function(asset, band) {
    ds$setDescription(
      band = band,
      description = asset
    )
  })

  if (using_daemons()) {
    mirai::everywhere({
      library(vrtility)
    })
  }

  # Create mirai map with promises
  jobs <- purrr::pmap(
    blocks_df,
    carrier::crate(
      function(...) {
        # Extract block parameters correctly from the 1-row dataframe
        block_params <- rlang::dots_list(...)

        inds <- methods::new(gdalraster::GDALRaster, vrt_file)
        on.exit(inds$close())
        # Read and combine bands
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

        print(inds$getScale(block_params[["band_n"]]))

        bscale <- inds$getScale(block_params[["band_n"]])
        if (!is.na(bscale)) {
          band_data <- band_data * bscale
        }

        return(list(
          band_data = band_data,
          block_params = block_params
        ))
      },
      vrt_file = vrt_template,
      compute_with_py_env = compute_with_py_env
    ),
    .parallel = using_daemons(),
    .progress = !quiet
  )

  purrr::walk(jobs, function(j) {
    ds$write(
      band = j$block_params[["band_n"]],
      xoff = j$block_params[["nXOff"]],
      yoff = j$block_params[["nYOff"]],
      xsize = j$block_params[["nXSize"]],
      ysize = j$block_params[["nYSize"]],
      rasterData = j$band_data
    )
  })

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
