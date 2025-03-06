#' Set mask band of a VRT collection
#'
#' @export
#' @rdname vrt_set_maskfun
vrt_set_maskfun <- function(
  x,
  valid_bits,
  mask_pix_fun = vrtility::bitmask_numba,
  drop_mask_band = TRUE,
  ...
) {
  UseMethod("vrt_set_maskfun")
}

#' @noRd
#' @export
vrt_set_maskfun.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_set_maskfun()')} not implemented for class {class(x)[1]}"
  )
}

#' @param x A VRT block
#' @param mask_band The name of the mask band
#' @param valid_bits A numeric vector of valid bits
#' @param mask_pix_fun A function that returns the Python code for the mask
#' pixel function
#' @param drop_mask_band Logical. If TRUE, the mask band will be removed from
#' the VRT block.
#' @return A VRT block with the mask band set.
#' @export
#' @rdname vrtility-internal
#' @keywords internal
vrt_set_maskfun.vrt_block <- function(
  x,
  valid_bits,
  mask_band,
  mask_pix_fun = vrtility::bitmask_numba,
  drop_mask_band = TRUE
) {
  v_assert_type(mask_band, "mask_band", "character", nullok = FALSE)
  v_assert_type(valid_bits, "valid_bits", "numeric", nullok = FALSE)
  v_assert_type(mask_pix_fun, "mask_pix_fun", "function", nullok = FALSE)

  vx <- xml2::read_xml(x$vrt)

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")
  descs <- purrr::map_chr(
    bands,
    ~ xml2::xml_text(xml2::xml_find_first(.x, ".//Description"))
  )

  mask_idx <- which(descs == mask_band)

  if (length(mask_idx) == 0) {
    cli::cli_abort(c(
      "Could not find mask band: {.val {mask_band}}",
      "i" = "Available bands: {.val {descs}}"
    ))
  }

  mskvrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  ts <- save_vrt(x)
  ds <- new(gdalraster::GDALRaster, ts)
  band_files <- setdiff(ds$getFileList(), ds$getFilename())
  msk_file <- band_files[mask_idx]

  gdalraster::buildVRT(mskvrt, msk_file, quiet = TRUE)

  msk_vrt_xml <- xml2::read_xml(mskvrt)
  msk_band <- xml2::xml_find_first(msk_vrt_xml, ".//VRTRasterBand")
  drop_nodatavalue(msk_band)
  xml2::xml_set_attr(msk_band, "subClass", "VRTDerivedRasterBand")
  xml2::xml_add_child(msk_band, "PixelFunctionType", "bitmask")
  xml2::xml_add_child(msk_band, "PixelFunctionLanguage", "Python")
  pf_args <- xml2::xml_add_child(msk_band, "PixelFunctionArguments")
  xml2::xml_set_attr(
    pf_args,
    "valid_values",
    paste(valid_bits, collapse = ",")
  )
  cdata_node <- xml2::xml_cdata(build_bitmask_numpy())
  pixel_func_code <- xml2::xml_add_child(msk_band, "PixelFunctionCode")
  xml2::xml_add_child(pixel_func_code, cdata_node)

  wrp_msk_pf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(msk_vrt_xml, wrp_msk_pf)
  wmxmlsrc <- xml2::read_xml(as.character(
    xml2::xml_find_first(
      bands[[mask_idx]],
      ".//SimpleSource | .//ComplexSource"
    )
  ))

  source_filename <- xml2::xml_find_first(wmxmlsrc, ".//SourceFilename")
  xml2::xml_set_text(source_filename, fs::path_file(wrp_msk_pf))

  # update all other bands
  purrr::walk(bands[-mask_idx], function(.x) {
    xml2::xml_add_child(.x, wmxmlsrc)

    no_data <- xml2::xml_find_first(.x, ".//NoDataValue") |>
      xml2::xml_double()

    xml2::xml_set_attr(.x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(.x, "PixelFunctionType", "bitmask")
    xml2::xml_add_child(.x, "PixelFunctionLanguage", "Python")
    pf_args <- xml2::xml_add_child(.x, "PixelFunctionArguments")
    xml2::xml_set_attr(
      pf_args,
      "valid_values",
      paste(valid_bits, collapse = ",")
    )
    xml2::xml_set_attr(pf_args, "no_data_value", no_data)

    cdata_node <- xml2::xml_cdata(mask_pix_fun())
    pixel_func_code <- xml2::xml_add_child(.x, "PixelFunctionCode")
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  if (drop_mask_band) {
    xml2::xml_remove(bands[[mask_idx]])
  }

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)
  build_vrt_block(tf, maskfun = mask_pix_fun(), pixfun = x$pixfun)
}

#' @param x A VRT collection
#' @rdname vrt_set_maskfun
#' @export
#' @return A VRT collection with the mask band set.
#' @export
vrt_set_maskfun.vrt_collection <- function(
  x,
  valid_bits,
  mask_pix_fun = vrtility::bitmask_numba,
  drop_mask_band = TRUE
) {
  mask_band <- check_mask_band(x)
  purrr::map(
    x$vrt,
    ~ vrt_set_maskfun(.x, valid_bits, mask_band, mask_pix_fun, drop_mask_band)
  ) |>
    build_vrt_collection(maskfun = mask_pix_fun(), pixfun = x$pixfun)
}


check_mask_band <- function(x) {
  if (nchar(x$mask_band_name) == 0) {
    cli::cli_abort(c(
      "!" = "No mask band is assigned.",
      "i" = "This must be set in `vrt_collect()`"
    ))
  }
  return(x$mask_band_name)
}
