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

  # new gdalraster raises warnings here... although I think we can ignore.
  gdalraster::warp(
    src_files,
    outfile,
    t_srs = t_srs,
    cl_arg = cl_arg,
    quiet = quiet
  )

  return(normalizePath(outfile))
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
  dst_nodata = NULL,
  add_args = NULL
) {
  opts_check(warp_opts, "-te")
  opts_check(warp_opts, "-tr")
  opts_check(warp_opts, "-r")

  warp_opts <- c(
    as.vector(rbind("-co", creation_options)),
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

  if (!is.null(dst_nodata)) {
    warp_opts <- c(warp_opts, "-dstnodata", dst_nodata)
  }

  if (!is.null(add_args)) {
    warp_opts <- c(warp_opts, add_args)
  }

  return(warp_opts)
}
