#' @importFrom methods new
#' @importFrom gdalraster GDALRaster
call_gdalraster_mirai <- function(
  x,
  outfile = fs::file_temp(ext = "tif"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = FALSE,
  nsplits = NULL
) {
  daemon_setup(gdal_config = config_options)
  orig_config <- set_gdal_config(c(config_options))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  rt <- raster_template_params(x)
  vrt_template <- rt$vrt_template

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

    if (!is.na(rt$scale_vals[band])) {
      ds$setScale(
        band = band,
        scale = rt$scale_vals[band]
      )
    }
  })

  if (mirai::daemons_set()) {
    async_gdalreader_band_read_write(
      blocks_df,
      vrt_file = vrt_template,
      ds = ds
    )
  } else {
    sequential_gdalreader_band_read_write(
      blocks_df,
      vrt_file = vrt_template,
      ds = ds,
      quiet = quiet
    )
  }

  return(outfile)
}
