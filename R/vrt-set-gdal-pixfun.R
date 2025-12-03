#' Set built-in GDAL pixel functions of a VRT stack object
#' @param x A vrt_stack object
#' @param pixfun A built-in GDAL pixel function. See details for a list of
#' available functions.
#' @param ... named arguments used within the pixel function see details.
#' @param band_idx The indices of the bands to set the pixel function for. If
#' NULL, the pixel function is set for all bands.
#' @param nodata_as_nan A logical indicating if the NoData value should be set to
#' NaN. Defaults to FALSE. Sometimes useful if the pixel functions ignore the
#' NoData value. see https://github.com/OSGeo/gdal/issues/4746. If you use this
#' you will probably need to specify the '-srcnodata' option on compute.
#' @details
#' The documentation for the GDAL pixel functions can be found at
#' \url{https://gdal.org/en/stable/drivers/raster/vrt.html#built-in-pixel-functions}
#'
#' Where a pixel function requires arguments, these can be passed as named
#' arguments, in line with the specified naming in PixelFunctionArguments of
#' the table at the above link.
#' @export
#' @rdname vrt_set_gdal_pixelfun
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' ex_collect <- vrt_collect(s2files)
#'
#' t_block <- ex_collect[[1]][[1]]
#' ex_stack <- ex_collect |>
#'   vrt_set_maskfun(
#'     mask_band = "SCL",
#'     mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
#'     drop_mask_band = TRUE
#'   ) |>
#'   vrt_warp(
#'     t_srs = t_block$srs,
#'     te = t_block$bbox,
#'     tr = t_block$res
#'   ) |>
#'   vrt_stack()
#'
#' plot(
#'   vrt_set_gdal_pixelfun(
#'     ex_stack,
#'     pixfun = "min",
#'     propagateNoData = TRUE
#'   ),
#'   bands = c(3, 2, 1)
#' )
#'
#' plot(
#'   vrt_set_gdal_pixelfun(
#'     ex_stack,
#'     pixfun = "min"
#'   ),
#'   bands = c(3, 2, 1)
#' )
#'
vrt_set_gdal_pixelfun <- function(
  x,
  pixfun,
  ...,
  band_idx,
  nodata_as_nan
) {
  UseMethod("vrt_set_gdal_pixelfun")
}

#' @noRd
#' @export
vrt_set_gdal_pixelfun.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{cli::code_highlight('vrt_set_gdal_pixelfun()')}
    not implemented for class {class(x)[1]}",
      "i" = "x must be a vrt_stack object."
    )
  )
}

#' @export
#' @rdname vrt_set_gdal_pixelfun
vrt_set_gdal_pixelfun.vrt_block <- function(
  x,
  pixfun,
  ...,
  band_idx = NULL,
  nodata_as_nan = FALSE
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

  purrr::walk(bands[band_idx], ~ set_gdal_pixfun_xml(.x, pixfun, pf_arg_vals))

  if (nodata_as_nan) {
    set_nodatavalue(vx, "NaN", nodata_targets = ".//NoDataValue")
  }

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  if (inherits(x, "vrt_stack_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  if (inherits(x, "vrt_stack")) {
    build_vrt_stack(
      vrt_to_vrt(tf),
      n_items = x$n_items,
      maskfun = x$maskfun,
      pixfun = pixfun,
      warped = warped
    )
  } else {
    build_vrt_block(
      vrt_to_vrt(tf),
      maskfun = x$maskfun,
      pixfun = pixfun,
      warped = warped,
      is_remote = x$is_remote
    )
  }
}

#' @export
#' @rdname vrt_set_gdal_pixelfun
vrt_set_gdal_pixelfun.vrt_collection <- function(
  x,
  pixfun,
  ...,
  band_idx = NULL,
  nodata_as_nan = FALSE
) {
  blocks_with_gdal_pf <- purrr::map(
    x$vrt,
    \(.x) {
      vrt_set_gdal_pixelfun(
        .x,
        pixfun = pixfun,
        ...,
        band_idx = band_idx
      )
    }
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


set_gdal_pixfun_xml <- function(band_xml, pixfun, pf_arg_vals = list()) {
  check_for_pixel_fun(band_xml)
  xml2::xml_set_attr(band_xml, "subClass", "VRTDerivedRasterBand")
  xml2::xml_add_child(band_xml, "PixelFunctionType", pixfun)
  if (length(pf_arg_vals) > 0) {
    pf_args <- xml2::xml_add_child(band_xml, "PixelFunctionArguments")
    purrr::iwalk(pf_arg_vals, function(val, name) {
      if (name == "expression") {
        xml2::xml_set_attr(pf_args, name, xml2::xml_cdata(val)) # perhaps not needed
      } else {
        xml2::xml_set_attr(pf_args, name, val)
      }
    })
  }
}
