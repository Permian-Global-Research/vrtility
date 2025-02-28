#' Stack VRT files from a vrt_collection object
#' @param x A vrt_collection object
#' @param ... Additional arguments passed to methods
#' @export
#' @rdname vrt_classes
vrt_stack <- function(x, ...) {
  UseMethod("vrt_stack")
}


#' @noRd
#' @export
vrt_stack.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_stack()')} not implemented for class {class(x)[1]}"
  )
}

#' @export
#' @param quiet Logical. If TRUE, suppress GDAL progress bar
#' @rdname vrt_classes
vrt_stack.vrt_collection <- function(x, quiet = TRUE, ...) {
  vrt_paths <- purrr::map_chr(
    x[[1]],
    function(.x) {
      tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
      vrt <- xml2::read_xml(.x$vrt)
      xml2::write_xml(vrt, tf)
      return(tf)
    }
  )

  main_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  gdalraster::buildVRT(
    vrt_filename = main_vrt,
    input_rasters = vrt_paths,
    cl_arg = c("-allow_projection_difference"),
    quiet = quiet
  )

  main_vrt <- set_vrt_descriptions(
    main_vrt,
    x$assets,
    as_file = TRUE
  ) #|>
  main_vrt <- set_vrt_metadata(
    main_vrt,
    keys = paste0("datetime_", seq_along(x$date_time)),
    values = x$date_time,
    as_file = TRUE
  )

  build_vrt_stack(main_vrt, pix_fun = x$pixfun, maskfun = x$maskfun)
}
