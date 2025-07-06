#' @title set the scale values for a VRT_x object
#' @description Set the Scale XML tag for a VRT_x object. This information is
#' preserved in derived VRT and GeoTIFF files.
#' @param x A VRT_x object
#' @param scale_value a numeric vector of length 1 or equal to the number of
#' bands in the VRT_x object.
#' @param offset_value a numeric value to set the Offset XML tag for the VRT_x
#' object. Default is 0.
#' @param band_idx  numeric; the target band position(s) to set the scale value
#' for. If NULL, The scale_value will be set for all bands.
#' @details
#' This sets the Scale and Offset XML tags for the VRT_x object. This is
#' useful for scaling the pixel values of the VRT_x object. This is however,
#' a little confusing... If your sources are ComplexSources, then this function
#' applies the scale and offset to the ComplexSource elements in addition to the
#' raster bands. If the sources are SimpleSources (which don't support)
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' ex_collect <- vrt_collect(s2files)
#' ex_sc1 <- vrt_set_scale(
#'   ex_collect,
#'   scale_value = 1e-4
#' )
#' print(ex_sc1[[1]][[1]], xml = TRUE)
#' @export
vrt_set_scale <- function(x, scale_value, offset_value, band_idx) {
  UseMethod("vrt_set_scale")
}

#' @noRd
#' @keywords internal
#' @export
vrt_set_scale.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_set_scale method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
vrt_set_scale.vrt_block <- function(
  x,
  scale_value,
  offset_value = 0.0,
  band_idx = NULL
) {
  purrr::pwalk(
    list(
      v = list(scale_value, offset_value),
      n = list("scale_value", "offset_value")
    ),
    function(v, n) {
      if (length(v) > 1) {
        v_assert_length(
          v,
          n,
          length(x$assets),
          nullok = FALSE
        )
      }
    }
  )

  # Get the VRT XML
  vrt_xml <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  if (is.null(band_idx)) {
    band_idx <- seq_along(bands)
  }

  purrr::pwalk(
    list(band = bands[band_idx], scale = scale_value, offset = offset_value),
    function(band, scale, offset) {
      # Remove existing Scale elements
      reset_element(band, "Scale", scale)

      # Remove existing Offset elements
      reset_element(band, "Offset", offset)

      band_src <- xml2::xml_find_first(
        band,
        ".//ComplexSource"
      )
      # Remove existing ScaleOffset elements
      reset_element(band_src, "ScaleOffset", offset)
      reset_element(band_src, "ScaleRatio", scale)

      xml2::xml_set_attr(band, "dataType", "Float32")
    }
  )
  out_vrt <- fs::file_temp(
    tmp_dir = getOption("vrt.cache"),
    ext = "vrt"
  )
  # Save the modified VRT XML back to the file
  xml2::write_xml(vrt_xml, out_vrt)

  build_vrt_block(
    out_vrt,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = x$warped
  )
}


#' @export
vrt_set_scale.vrt_collection <- function(
  x,
  scale_value,
  offset_value = 0.0,
  band_idx = NULL
) {
  block_list <- purrr::map(
    x[[1]],
    ~ vrt_set_scale(
      .x,
      scale_value = scale_value,
      offset_value = offset_value,
      band_idx = band_idx
    )
  )
  build_vrt_collection(
    block_list,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = x$warped
  )
}
