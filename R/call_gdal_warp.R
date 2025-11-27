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
    src_files = src_files,
    dst_filename = outfile,
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
  resampling = NULL,
  te = NULL,
  res = NULL,
  dst_nodata = NULL,
  add_args = NULL
) {
  opts_check(warp_opts, "-te")
  opts_check(warp_opts, "-tr")
  opts_check(warp_opts, "-r")

  warp_cl_args <- c(
    split_of_and_co(creation_options),
    warp_opts
  )

  if (!is.null(resampling)) {
    warp_cl_args <- c(warp_cl_args, c("-r", resampling))
  }

  if (!is.null(te)) {
    warp_cl_args <- c(
      warp_cl_args,
      "-te",
      te
    )
  }

  if (!is.null(res)) {
    warp_cl_args <- c(
      warp_cl_args,
      "-tr",
      res,
      if ("-tap" %in% warp_cl_args) NULL else "-tap"
    )
  }

  if (!is.null(dst_nodata)) {
    warp_cl_args <- c(warp_cl_args, "-dstnodata", dst_nodata)
  }

  if (!is.null(add_args)) {
    warp_cl_args <- c(warp_cl_args, add_args)
  }

  return(warp_cl_args)
}


split_of_and_co <- function(creation_options) {
  if ("-of" %in% creation_options) {
    ofkeyidx <- which(creation_options == "-of")
    out_format <- creation_options[ofkeyidx + 1]
    ofkeypair <- c("-of", out_format)
    creation_options <- creation_options[-c(ofkeyidx, ofkeyidx + 1)]
  } else {
    ofkeypair <- NULL
  }

  if (rlang::is_empty(creation_options)) {
    return(ofkeypair)
  }

  return(c(
    ofkeypair,
    as.vector(rbind("-co", creation_options))
  ))
}
