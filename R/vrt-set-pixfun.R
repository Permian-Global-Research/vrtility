#' Set the pixel function of a VRT stack object
#' @param x A vrt_stack object
#' @param pixfun A function that returns the Python code for the pixel function
#' @export
#' @rdname vrt_set_pixelfun
#'
vrt_set_pixelfun <- function(x, pixfun = vrtility::median_numba()) {
  UseMethod("vrt_set_pixelfun")
}

#' @noRd
#' @export
vrt_set_pixelfun.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{cli::code_highlight('vrt_set_pixelfun()')}
    not implemented for class {class(x)[1]}",
      "i" = "x must be a vrt_stack object."
    )
  )
}

#' @export
#' @rdname vrt_set_pixelfun
vrt_set_pixelfun.vrt_stack <- function(
  x,
  pixfun = vrtility::median_numpy()
) {
  v_assert_type(pixfun, "pixfun", "character", nullok = FALSE)

  vx <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  purrr::walk(bands, function(.x) {
    no_data <- xml2::xml_find_first(.x, ".//NoDataValue") |>
      xml2::xml_double()

    xml2::xml_set_attr(.x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(.x, "PixelFunctionType", "pixfun")
    xml2::xml_add_child(.x, "PixelFunctionLanguage", "Python")
    pf_args <- xml2::xml_add_child(.x, "PixelFunctionArguments")
    xml2::xml_set_attr(pf_args, "no_data_value", no_data)

    cdata_node <- xml2::xml_cdata(pixfun)
    pixel_func_code <- xml2::xml_add_child(.x, "PixelFunctionCode")
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  if (inherits(x, "vrt_stack_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  build_vrt_stack(tf, maskfun = x$maskfun, pixfun = pixfun, warped = warped)
}
