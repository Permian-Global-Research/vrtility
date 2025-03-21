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

#' @noRd
#' @keywords internal
call_rioxarray_dask <- function(wf, outfile, dask_workers) {
  dask.distributed <- reticulate::import("dask.distributed")
  rioxarray <- reticulate::import("rioxarray")
  set_gdal_config(gdal_config_opts(), scope = "sys")
  with(dask.distributed$LocalCluster(n_workers = dask_workers) %as% cluster, {
    with(dask.distributed$Client(cluster) %as% client, {
      cli::cli_alert_info(
        "Dask dashboard @: {client$dashboard_link}"
      )
      xds <- rioxarray$open_rasterio(
        wf,
        chunks = TRUE,
        lock = FALSE,
        # lock=Lock("rio-read", client=client), # when too many file handles open
      )
      xds$rio$to_raster(
        outfile,
        tiled = TRUE,
        lock = dask.distributed$Lock("rio", client = client),
      )
    })
  })
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
