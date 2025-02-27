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

#' @param x A VRT block
#' @param mask_band The name of the mask band
#' @param valid_bits A numeric vector of valid bits
#' @param mask_pix_fun A function that returns the Python code for the mask
#' pixel function
#' @return A VRT block with the mask band set.
#' @export
#' @rdname vrt_set_mask
vrt_set_mask.vrt_block <- function(
  x,
  mask_band,
  valid_bits,
  mask_pix_fun = vrtility::bitmask_numba
) {
  v_assert_type(mask_band, "mask_band", "character", nullok = FALSE)
  v_assert_type(valid_bits, "valid_bits", "numeric", nullok = FALSE)
  v_assert_type(mask_pix_fun, "mask_pix_fun", "function", nullok = FALSE)

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

  mask_src <- xml2::xml_find_all(
    bands[[mask_idx]],
    ".//SimpleSource | .//ComplexSource"
  )
  mask_src_chr <- xml2::read_xml(as.character(mask_src))

  purrr::walk(bands[-mask_idx], function(.x) {
    xml2::xml_add_child(.x, mask_src_chr)

    xml2::xml_set_attr(.x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(.x, "PixelFunctionType", "sentinel2_mask")
    xml2::xml_add_child(.x, "PixelFunctionLanguage", "Python")
    pf_args <- xml2::xml_add_child(.x, "PixelFunctionArguments")
    xml2::xml_set_attr(
      pf_args,
      "valid_values",
      paste(valid_bits, collapse = ",")
    )

    cdata_node <- xml2::xml_cdata(mask_pix_fun())
    pixel_func_code <- xml2::xml_add_child(.x, "PixelFunctionCode")
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)
  build_vrt_block(tf, maskfun = mask_pix_fun(), pixfun = x$pixfun)
}

#' @param x A VRT collection
#' @rdname vrt_set_mask
#' @export
#' @return A VRT collection with the mask band set.
#' @export
vrt_set_mask.vrt_collection <- function(
  x,
  mask_band,
  valid_bits,
  mask_pix_fun = vrtility::bitmask_numba
) {
  # browser()
  purrr::map(x$vrt, ~ vrt_set_mask(.x, mask_band, valid_bits, mask_pix_fun)) |>
    build_vrt_collection(maskfun = mask_pix_fun(), pixfun = x$pixfun)
}
