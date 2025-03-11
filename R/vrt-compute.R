#' Generate a composite raster from (virtual) raster sources.
#' @param x A vrt_block, vrt_stack, or vrt_collection object
#' @param outfile A character string of the output file path
#' @param t_srs A character string of the target SRS
#' @param te A numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) and must be the same SRS as in `t_srs`.
#' @param tr A numeric vector of the target resolution in the form c(xres, yres)
#' @param warp_options A character vector of options to pass to the warp
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
#' @export
#' @rdname vrt_compute
#' @details This is the primary function to call processing of raster data. The
#' behaviour of the warper is dependent on the form of the input vrt datasets
#' and the associated options.
vrt_compute <- function(
  x,
  outfile,
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  v_assert_type(outfile, "outfile", "character")
  v_assert_length(te, "te", 4)

  UseMethod("vrt_compute")
}

#' @noRd
#' @export
vrt_compute.default <- function(x, ...) {
  cli::cli_abort(c("vrt_compute() not implemented for class {class(x)[1]}"))
}

#' @export
#' @rdname vrt_compute
vrt_compute.vrt_block <- function(
  x,
  outfile,
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  v_assert_length(tr, "tr", 2)
  # browser()
  tmp_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  file_src <- xml2::read_xml(x$vrt)
  xml2::write_xml(file_src, tmp_vrt)

  warp_options <- combine_warp_opts(warp_options, te, tr)

  compute_with_py_env(
    call_vrt_compute(
      src_files = tmp_vrt,
      outfile = outfile,
      t_srs = t_srs,
      warp_options = warp_options,
      config_options = config_options,
      quiet = quiet
    )
  )
}

#' @export
#' @rdname vrt_compute
vrt_compute.vrt_stack_warped <- function(
  x,
  outfile,
  t_srs,
  te,
  tr,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  if (any(missing(t_srs), missing(te), missing(tr))) {
    cli::cli_abort(
      c(
        "The following arguments are required for a `vrt_stacked_warp` object:",
        ">" = (paste(c("`t_srs`", "`te`", "`tr`"), collapse = ", "))
      )
    )
  }
  NextMethod()
}

#' @export
#' @rdname vrt_compute
vrt_compute.vrt_collection <- function(
  x,
  outfile,
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  v_assert_length(tr, "tr", 2)

  uniq_pths <- purrr::imap_chr(
    x[[1]],
    function(.x, .y) {
      if (nchar(.x$date_time) > 0) .x$date_time else .y
    }
  ) |>
    unique_fp(outfile)

  purrr::map2_chr(
    x[[1]],
    uniq_pths,
    function(.x, .y) {
      vrt_compute(
        .x,
        outfile = .y,
        t_srs = t_srs,
        te = te,
        warp_options = warp_options,
        config_options = config_options,
        quiet = TRUE
      )
    },
    .progress = !quiet
  )
}


#' Set the GDAL configuration options
#' @param x A named character vector of the configuration options
#' @param unset A logical indicating whether to unset the options
#' @keywords internal
#' @noRd
set_config <- function(x) {
  # Store original values
  original_values <- purrr::map_chr(
    names(x),
    ~ gdalraster::get_config_option(.x)
  ) |>
    purrr::set_names(names(x))

  # Set the config options
  purrr::iwalk(x, ~ gdalraster::set_config_option(.y, .x))
  invisible(original_values)
}


#' A very thin wrapper around the `gdalraster::warp` function
#' @keywords internal
#' @noRd
call_vrt_compute <- function(
  src_files,
  outfile,
  t_srs,
  warp_options,
  config_options,
  quiet = FALSE
) {
  v_assert_type(src_files, "src_files", "character")
  v_assert_type(outfile, "outfile", "character")
  v_assert_type(t_srs, "t_srs", "character")
  v_assert_type(warp_options, "warp_options", "character")
  v_assert_type(config_options, "config_options", "character")

  orig_config <- set_config(config_options)
  on.exit(set_config(orig_config))

  gdalraster::warp(
    src_files,
    outfile,
    t_srs = t_srs,
    cl_arg = warp_options,
    quiet = quiet
  )

  return(outfile)
}

combine_warp_opts <- function(warp_opts, te, res = NULL) {
  opts_check(warp_opts, "-te")
  opts_check(warp_opts, "-tr")

  warp_opts <- c(
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

  return(warp_opts)
}
