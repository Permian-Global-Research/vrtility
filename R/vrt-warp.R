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
#' @rdname vrt_warp
#' @details This is the primary function to call processing of raster data. The
#' behaviour of the warper is dependent on the form of the input vrt datasets
#' and the associated options.
vrt_warp <- function(
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

  UseMethod("vrt_warp")
}

#' @noRd
#' @export
vrt_warp.default <- function(x, ...) {
  cli::cli_abort(c("vrt_warp() not implemented for class {class(x)[1]}"))
}

#' @export
#' @rdname vrt_warp
vrt_warp.vrt_block <- function(
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
  tmp_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  file_src <- xml2::read_xml(x$vrt)
  xml2::write_xml(file_src, tmp_vrt)

  warp_options <- combine_warp_opts(warp_options, te, tr)

  vrt_warp_method(
    x = tmp_vrt,
    outfile = outfile,
    t_srs = t_srs,
    warp_options = warp_options,
    config_options = config_options,
    quiet = quiet
  )
}

#' @export
#' @rdname vrt_warp
vrt_warp.vrt_collection <- function(
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
      vrt_warp(
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

#' A wrapper to call the warper from the s3 methods
#' @noRd
#' @keywords internal
vrt_warp_method <- function(
  x,
  outfile,
  t_srs,
  te,
  res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  # First, ensure we have the correct paths
  py_bin <- Sys.getenv("VRTILITY_PY_EXECUTABLE", unset = NA)

  if (is.na(py_bin)) {
    cli::cli_abort(c(
      "Cannot locate the {cli::style_bold('VRTILITY_PYTHON')} environment",
      "i" = "You may need to run
        {cli::code_highlight('`build_vrtility_python()`')} to install it"
    ))
  }

  py_env <- dirname(dirname(py_bin))

  # Modified environment setup
  withr::with_envvar(
    new = c(
      # Only set essential variables
      "RETICULATE_PYTHON" = py_bin,
      "VIRTUALENV" = py_env,
      "PATH" = paste(fs::path(py_env, "bin"), Sys.getenv("PATH"), sep = ":")
    ),
    code = {
      # Initialize reticulate first
      reticulate::use_python(py_bin, required = TRUE)

      call_vrt_warp(
        src_files = x,
        outfile = outfile,
        t_srs = t_srs,
        warp_options = warp_options,
        config_options = config_options,
        quiet = quiet
      )
    }
  )
}


#' Set the GDAL configuration options
#' @param x A named character vector of the configuration options
#' @param unset A logical indicating whether to unset the options
#' @keywords internal
#' @noRd
set_config <- function(x, unset = FALSE) {
  # Store original values
  original_values <- purrr::map_chr(
    names(x),
    ~ gdalraster::get_config_option(.x)
  ) |>
    purrr::set_names(names(x))

  # If unset, use original values, otherwise use input values
  values_to_set <- if (unset) original_values else x

  # Set the config options
  purrr::iwalk(values_to_set, ~ gdalraster::set_config_option(.y, .x))
}


#' A very thin wrapper around the `gdalraster::warp` function
#' @keywords internal
#' @noRd
call_vrt_warp <- function(
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

  set_config(config_options)
  on.exit(set_config(config_options, unset = TRUE))

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
