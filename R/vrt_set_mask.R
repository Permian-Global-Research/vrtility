#' Set mask band of a VRT collection
#'
#' @export
#' @rdname vrt_set_mask
vrt_set_mask <- function(x, ...) {
  UseMethod("vrt_set_mask")
}

#' @noRd
#' @export
vrt_set_mask.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_set_mask()')} not implemented for class {class(x)[1]}"
  )
}

#' @export
#' @rdname vrt_set_mask
vrt_set_mask.vrt_block <- function(x, mask_band) {
  vx <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")

  descs <- purrr::map_chr(
    bands,
    ~ xml2::xml_attr(.x, "Description", default = "")
  )

  mask_idx <- which(descs == mask_band)

  if (length(mask_idx) == 0) {
    cli::cli_abort(c(
      "Could not find mask band: {.val {mask_band}}",
      "i" = "Available bands: {.val {descs}}"
    ))
  }

  mask_band_node <- bands[[mask_idx]]

  # Set pixel function attributes
  xml2::xml_set_attr(mask_band_node, "subClass", "VRTDerivedRasterBand")
  xml2::xml_add_child(mask_band_node, "PixelFunctionType", "sentinel2_mask")
  xml2::xml_add_child(mask_band_node, "PixelFunctionLanguage", "Python")

  # Add pixel function code that handles SCL values
  pixel_func <- glue::glue(
    "
import numpy as np
breakpoint()
def sentinel2_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    # Valid SCL values:
    # 4 = Vegetation
    # 5 = Not vegetated
    # 6 = Water
    # 7 = Unclassified
    # 11 = Snow
    valid_values = [4, 5, 6, 7, 11]
    
    breakpoint()
    # Set mask to 1 (valid) for valid pixels, 0 (invalid) for others
    mask = np.isin(in_ar[0], valid_values)
    out_ar[:] = mask.astype(np.uint8)
"
  )

  # Add pixel function code as CDATA
  cdata_node <- xml2::xml_cdata(pixel_func)
  pixel_func_code <- xml2::xml_add_child(mask_band_node, "PixelFunctionCode")
  xml2::xml_add_child(pixel_func_code, cdata_node)

  # browser()
  # cat(as.character(vx))

  # Remove the mask band from its current position
  # Create MaskBand element and add the VRTRasterBand to it

  ndv <- xml2::xml_find_all(mask_band_node, "NoDataValue")
  xml2::xml_remove(ndv)
  masksrc <- xml2::xml_find_all(
    mask_band_node,
    "//SimpleSource | //ComplexSource"
  )
  xml2::xml_set_name(masksrc, "SimpleSource")
  ndc <- xml2::xml_find_all(masksrc, "NODATA")
  xml2::xml_remove(ndc)
  xml2::xml_remove(mask_band_node)

  main_bands <- xml2::xml_find_all(
    vx,
    "//SimpleSource | //ComplexSource"
  )
  purrr::walk(main_bands, function(x) {
    nodatachild <- xml2::xml_find_all(x, "NODATA")
    xml2::xml_remove(nodatachild)
    use_mask <- xml2::xml_add_child(x, "UseMaskBand")
    xml2::xml_text(use_mask) <- "true"
  })
  mask_band_xml <- xml2::xml_add_child(vx, "MaskBand")
  xml2::xml_add_child(mask_band_xml, mask_band_node)

  # Write back to block
  x$vrt <- as.character(vx)
  return(x)
}
