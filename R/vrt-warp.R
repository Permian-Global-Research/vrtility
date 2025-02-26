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
  purrr::iwalk(
    values_to_set,
    ~ gdalraster::set_config_option(.y, .x)
  )
}


#' A very thin wrapper.
#' @param src_files A stac_vrt object or a character vector of file paths to the
#' rasters
#' @param outfile A character string of the output file path
#' @param t_srs A character string of the target SRS
#' @param vrt_options A list of options to pass to the VRT
#' @param warp_options A character vector of options to pass to the warp
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
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
  # TODO: this wont warn about stac_vrt so add at some point.
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


combine_warp_opts <- function(warp_opts, te, res) {
  opts_check(warp_opts, "-te")
  opts_check(warp_opts, "-tr")

  warp_opts <- c(
    warp_opts,
    "-te",
    paste(te, collapse = " "),
    "-tr",
    paste(res, collapse = " ")
  )

  return(warp_opts)
}


#' Generate a composite raster from (virtual) raster sources.
#' @param src_files A stac_vrt object or a character vector of file paths to the
#' rasters
#' @param outfile A character string of the output file path
#' @param bbox A numeric vector of the bounding box (length 4) in lat/long
#' @param fun A character string of the pixel function to apply (only median is
#' supported for now...)
#' @param t_srs A character string of the target SRS
#' @param res A numeric of the target resolution in units of t_srs
#' @param vrt_options A list of options to pass to the VRT
#' @param warp_options A character vector of options to pass to the warp
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
#' @export
#' @rdname vrt_warp
#' @details This is the main package function at present. It is quite targetted
#' towards Sentinel 2 data and will likely change - but this will server as the
#' main testing point for now!
vrt_warp <- function(
  x,
  outfile,
  t_srs,
  te,
  tr,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE,
  ...
) {
  UseMethod("vrt_warp")
}

#' @noRd
#' @export
vrt_warp.default <- function(
  x,
  ...
) {
  cli::cli_abort(
    c(
      "vrt_warp() not implemented for class {class(x)[1]}"
    )
  )
}

#' @export
#' @rdname vrt_warp
vrt_warp.vrt_block <- function(
  x,
  outfile,
  t_srs,
  te,
  tr = x$res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
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
  t_srs,
  te,
  tr = x$res,
  warp_options = getOption("vrt.gdal.warp.options"),
  config_options = getOption("vrt.gdal.config.options"),
  quiet = FALSE
) {
  browser()

  b <- x[[1]]
  class(x) <- "list"

  class(b)
  b

  purrr::map(
    x,
    function(.x) {
      purrr::map_chr(
        .x,
        ~ .x$date_time
      )
    }
  )

  vrt_warp(
    x,
    outfile = outfile,
    t_srs = t_srs,
    te = te,
    warp_options = warp_options,
    config_options = config_options,
    quiet = quiet
  )
}

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
    cli::cli_abort(
      c(
        "Cannot locate the {cli::style_bold('VRTILITY_PYTHON')} environment",
        "i" = "You may need to run
        {cli::code_highlight('`build_vrtility_python()`')} to install it"
      )
    )
  }

  py_env <- dirname(dirname(py_bin))

  # Modified environment setup
  withr::with_envvar(
    new = c(
      # Only set essential variables
      "RETICULATE_PYTHON" = py_bin,
      "VIRTUALENV" = py_env,
      "PATH" = paste(
        fs::path(py_env, "bin"),
        Sys.getenv("PATH"),
        sep = ":"
      )
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
      )
    }
  )
}
