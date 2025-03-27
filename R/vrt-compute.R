#' Generate a composite raster from (virtual) raster sources.
#' @param x A vrt_block, vrt_stack, or vrt_collection object
#' @param outfile A character string of the output file path
#' @param t_srs A character string of the target SRS
#' @param te A numeric vector of the target extent in the form
#' c(xmin, ymin, xmax, ymax) and must be the same SRS as in `t_srs`.
#' @param tr A numeric vector of the target resolution in the form c(xres, yres)
#' @param resampling A character vector of the resampling method to be used.
#' see details.
#' @param engine A character vector of the engine to use for processing the
#' raster data. See details.
#' @param warp_options A character vector of options to pass to gdalwarp
#' @param creation_options A character vector of options to pass to the
#' the gdal "engine".
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param nsplits An integer of the number of splits to use when using the
#' gdalraster engine.
#' @param add_cl_arg A character vector of additional command line arguments
#' that are not captured in `gdalwarp_options()` - these are not checked for
#' validity.
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
#' @export
#' @rdname vrt_compute
#' @details
#' The `resampling` default is "near", which should be chosen in vrt_warp has
#' already been used but "bilinear" may be prefereable where the input data is
#' has not yet been virtually aligned/resampled.
#'
#' The choice of `engine` will depend on the nature of the computation being
#' carried out. In the majority of cases warping is preferred, especically when
#' we are not processing the entirity of the input dataset (as is usually the
#' case when working with online data sources).
#'
vrt_compute <- function(
  x,
  outfile,
  t_srs,
  te,
  tr,
  resampling,
  engine,
  warp_options,
  creation_options,
  config_options,
  nsplits,
  add_cl_arg,
  quiet
) {
  v_assert_type(outfile, "outfile", "character")
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
  resampling = c(
    "near",
    "bilinear",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_opts(),
  nsplits = 1L,
  add_cl_arg = NULL,
  quiet = TRUE
) {
  v_assert_length(te, "te", 4)
  v_assert_length(tr, "tr", 2)
  v_assert_type(outfile, "outfile", "character")
  v_assert_type(t_srs, "t_srs", "character")
  v_assert_type(warp_options, "warp_options", "character")
  v_assert_type(creation_options, "creation_options", "character")
  v_assert_type(config_options, "config_options", "character")

  resampling <- rlang::arg_match(resampling)
  engine <- rlang::arg_match(engine)

  tmp_vrt <- normalizePath(vrt_save(x))

  if (engine == "warp") {
    cl_arg <- combine_warp_opts(
      creation_options,
      warp_options,
      resampling,
      te,
      tr,
      add_cl_arg
    )

    result <- compute_with_py_env(
      call_gdal_warp(
        src_files = tmp_vrt,
        outfile = outfile,
        t_srs = t_srs,
        config_options = config_options,
        cl_arg = cl_arg,
        quiet = quiet
      )
    )
  } else if (engine == "gdalraster") {
    if (!x$warped) warp_first_error(engine)

    # compute_with_py_env used internally.
    result <- call_gdalraster_mirai(
      src_files = tmp_vrt,
      outfile = outfile,
      resampling = resampling,
      cl_arg = c(creation_options, add_cl_arg),
      config_options = config_options,
      nsplits = nsplits,
      quiet = quiet
    )
  } else if (engine == "translate") {
    result <- compute_with_py_env(
      call_gdal_tanslate(
        src_files = tmp_vrt,
        outfile = outfile,
        config_options = config_options,
        cl_arg = c(creation_options, "-r", resampling),
        quiet = quiet
      )
    )
  }
  return(result)
}

#' @export
#' @rdname vrt_compute
vrt_compute.vrt_stack_warped <- function(
  x,
  outfile,
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  resampling = c(
    "near",
    "bilinear",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_opts(),
  nsplits = 1L,
  add_cl_arg = NULL,
  quiet = TRUE
) {
  class(x) <- setdiff(class(x), "vrt_stack_warped")
  vrt_compute(
    x = x,
    outfile = outfile,
    t_srs = t_srs,
    te = te,
    tr = tr,
    resampling = resampling,
    engine = engine,
    warp_options = warp_options,
    creation_options = creation_options,
    config_options = config_options,
    nsplits = nsplits,
    add_cl_arg = add_cl_arg,
    quiet = quiet
  )
}

#' @export
#' @rdname vrt_compute
vrt_compute.vrt_stack <- function(
  x,
  outfile,
  t_srs,
  te,
  tr,
  resampling = c(
    "near",
    "bilinear",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_opts(),
  nsplits = 1L,
  add_cl_arg = NULL,
  quiet = TRUE
) {
  if (any(missing(t_srs), missing(te), missing(tr))) {
    missing_args_error("vrt_stack")
  }
  NextMethod()
}


#' @export
#' @rdname vrt_compute
vrt_compute.vrt_collection_warped <- function(
  x,
  outfile,
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  resampling = c(
    "near",
    "bilinear",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_opts(),
  nsplits = 1L,
  add_cl_arg = NULL,
  quiet = FALSE
) {
  class(x) <- setdiff(class(x), "vrt_collection_warped")
  vrt_compute(
    x = x,
    outfile = outfile,
    t_srs = t_srs,
    te = te,
    tr = tr,
    resampling = resampling,
    engine = engine,
    warp_options = warp_options,
    creation_options = creation_options,
    config_options = config_options,
    nsplits = nsplits,
    add_cl_arg = add_cl_arg,
    quiet = quiet
  )
}


#' @export
#' @rdname vrt_compute
vrt_compute.vrt_collection <- function(
  x,
  outfile,
  t_srs,
  te,
  tr,
  resampling = c(
    "near",
    "bilinear",
    "cubic",
    "cubicspline",
    "lanczos",
    "average",
    "rms",
    "mode",
    "max",
    "min",
    "med",
    "q1",
    "q3",
    "sum"
  ),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_opts(),
  nsplits = 1L,
  add_cl_arg = NULL,
  quiet = FALSE
) {
  if (any(missing(t_srs), missing(te), missing(tr))) {
    missing_args_error("vrt_collection")
  }
  v_assert_length(tr, "tr", 2)

  uniq_pths <- purrr::imap_chr(
    unname(x[[1]]),
    function(.x, .y) {
      if (nchar(.x$date_time) > 0) .x$date_time else as.character(.y)
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
        tr = tr,
        resampling = resampling,
        engine = engine,
        warp_options = warp_options,
        creation_options = creation_options,
        config_options = config_options,
        nsplits = nsplits,
        add_cl_arg = add_cl_arg,
        quiet = TRUE
      )
    },
    .progress = !quiet
  )
}


#' @noRd
#' @keywords internal
missing_args_error <- function(x_class) {
  cli::cli_abort(
    c(
      "The following arguments are required for a `{x_class}` object:",
      ">" = (paste(c("`t_srs`", "`te`", "`tr`"), collapse = ", "))
    )
  )
}


warp_first_error <- function(engine) {
  cli::cli_abort(
    c(
      "The `{engine}` engine should only be used with warped VRTs",
      "i" = "Use `vrt_warp on the collection before`vrt_stack` or vrt_compute`"
    )
  )
}
