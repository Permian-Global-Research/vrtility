#' Set built-in GDAL pixel functions of a VRT stack object
#' @param x A vrt_stack object
#' @param pixfun A built-in GDAL pixel function. See details for a list of
#' available functions.
#' @param ... named arguments used within the pixel function see details.
#' @param band_idx The indices of the bands to set the pixel function for. If
#' NULL, the pixel function is set for all bands.
#' @details
#' The documentation for the GDAL pixel functions can be found at
#' \url{https://gdal.org/en/stable/drivers/raster/vrt.html#built-in-pixel-functions}
#' @export
#' @rdname vrt_set_gdal_pixfun
#'
vrt_set_gdal_pixfun <- function(x, pixfun, ..., band_idx) {
  UseMethod("vrt_set_gdal_pixfun")
}

#' @noRd
#' @export
vrt_set_gdal_pixfun.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{cli::code_highlight('vrt_set_gdal_pixfun()')}
    not implemented for class {class(x)[1]}",
      "i" = "x must be a vrt_stack object."
    )
  )
}

#' @export
#' @rdname vrt_set_gdal_pixfun
vrt_set_gdal_pixfun.vrt_block <- function(
  x,
  pixfun,
  ...,
  band_idx = NULL
) {
  v_assert_type(pixfun, "pixfun", "character", nullok = FALSE)
  v_assert_type(
    band_idx,
    "band_idx",
    c("numeric", "integer"),
    nullok = TRUE,
    multiple = TRUE
  )
  pf_arg_vals <- rlang::dots_list(...)

  if (length(pf_arg_vals) > 0) {
    # change TRUE to "true" and FALSE to "false"
    pf_arg_vals <- purrr::map(pf_arg_vals, function(.x) {
      if (inherits(.x, "logical")) {
        return(tolower(.x))
      } else {
        return(as.character(.x))
      }
    })
  }

  vx <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  if (is.null(band_idx)) {
    band_idx <- seq_along(bands)
  } else {
    if (!all(band_idx %in% seq_along(bands))) {
      cli::cli_abort(
        c(
          "x" = "`band_idx` must be a vector of integers
        between 1 and {length(bands)}"
        )
      )
    }
  }

  purrr::walk(bands[band_idx], function(.x) {
    check_for_pixel_fun(.x)
    xml2::xml_set_attr(.x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(.x, "PixelFunctionType", pixfun)
    if (length(pf_arg_vals) > 0) {
      pf_args <- xml2::xml_add_child(.x, "PixelFunctionArguments")
      purrr::iwalk(pf_arg_vals, function(val, name) {
        xml2::xml_set_attr(pf_args, name, val)
      })
    }
  })
  # browser()

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  if (inherits(x, "vrt_stack_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  if (inherits(x, "vrt_stack")) {
    build_vrt_stack(tf, maskfun = x$maskfun, pixfun = pixfun, warped = warped)
  } else {
    build_vrt_block(tf, maskfun = x$maskfun, pixfun = pixfun, warped = warped)
  }
}

#' @export
#' @rdname vrt_set_gdal_pixfun
vrt_set_gdal_pixfun.vrt_collection <- function(
  x,
  pixfun,
  ...,
  band_idx = NULL
) {
  blocks_with_gdal_pf <- purrr::map(
    x$vrt,
    \(.x)
      vrt_set_gdal_pixfun(
        .x,
        pixfun = pixfun,
        ...,
        band_idx = band_idx
      )
  )

  if (inherits(x, "vrt_collection_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  build_vrt_collection(
    blocks_with_gdal_pf,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = warped
  )
}
