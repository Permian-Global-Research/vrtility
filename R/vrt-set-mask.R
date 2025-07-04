#' Set mask band of a VRT collection
#' @param x A VRT type object (collection or block)
#' @param mask_values A numeric vector of integer or bit values to be masked.
#' @param mask_band The name of the mask band
#' @param build_mask_pixfun A character string of the Python code to build the
#' mask. Provided functions include [build_intmask()] and [build_bitmask()].
#' See details.
#' @param set_mask_pixfun A character string of the Python code to set the mask.
#' Provided functions include [set_mask_numpy()]. See
#' details.
#' @param drop_mask_band Logical. If TRUE, the mask band will be removed from
#' the VRT block.
#' @param cache_dir A character string of the directory to use for temporary
#' files. In general this should be left alone. main purpose is to manage cache
#' location when running asyncronously with mirai.
#' @export
#' @rdname vrt_set_maskfun
#' @details
#' The `build_mask_pixfun` function is used to build the mask band. Where the
#' mask band is a true bitmask and bit-wise operations are required, the
#' [build_bitmask()] function should be used. For integer-based masking, where
#' the mask band is provided as a single band with integer values, the
#' [build_intmask()] function should be used.
#'
#' The `set_mask_pixfun` function is used to apply the mask to the other bands.
#' In general [set_mask_numpy()] should be used, although a custom function can
#' be provided.
#'
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#'
#' ex_collect <- vrt_collect(s2files)
#' ex_collect |>
#'   vrt_set_maskfun(
#'     mask_band = "SCL",
#'     mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
#'     drop_mask_band = FALSE)
#'
vrt_set_maskfun <- function(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun,
  set_mask_pixfun,
  drop_mask_band,
  cache_dir
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


#' @return A VRT block with the mask band set.
#' @export
#' @rdname vrt_set_maskfun
vrt_set_maskfun.vrt_block <- function(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = vrtility::build_intmask(),
  set_mask_pixfun = vrtility::set_mask_numpy(),
  drop_mask_band = TRUE,
  cache_dir = getOption("vrt.cache")
) {
  if (rlang::is_missing(mask_band)) {
    mask_band <- "vrtility.mask.missing.mask"
  }

  v_assert_type(mask_band, "mask_band", "character", nullok = FALSE)
  v_assert_type(
    mask_values,
    "mask_values",
    c("numeric", "integer"),
    nullok = FALSE,
    multiple = TRUE
  )
  v_assert_type(build_mask_pixfun, "mask_pix_fun", "character", nullok = FALSE)
  v_assert_type(set_mask_pixfun, "set_mask_pixfun", "character", nullok = FALSE)

  vx <- xml2::read_xml(x$vrt)

  no_data <- xml2::xml_find_first(vx, ".//NoDataValue") |>
    xml2::xml_double()

  bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")
  descs <- purrr::map_chr(
    bands,
    ~ xml2::xml_text(xml2::xml_find_first(.x, ".//Description"))
  )

  if (mask_band == "vrtility.mask.missing.mask") {
    mask_idx <- attr(build_mask_pixfun, "required_bands")
    if (is.null(mask_idx)) {
      cli::cli_abort(c(
        "No mask band provided and 'build_mask_pixfun' does not specify the
        required band(s)."
      ))
    }
  } else {
    mask_idx <- which(descs == mask_band)
  }

  if (length(mask_idx) == 0) {
    cli::cli_abort(c(
      "Could not find mask band: {.val {mask_band}}",
      "i" = "Available bands: {.val {descs}}"
    ))
  }

  ts <- vrt_save(x)
  ds <- methods::new(gdalraster::GDALRaster, ts)
  band_files <- setdiff(ds$getFileList(), ds$getFilename())
  mskvrt <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")

  if (length(band_files) == 1) {
    gdalraster::buildVRT(
      mskvrt,
      band_files,
      cl_arg = unlist(purrr::map(mask_idx, ~ c("-b", .x))),
      quiet = TRUE
    )
  } else {
    msk_file <- band_files[mask_idx]
    gdalraster::buildVRT(mskvrt, msk_file, quiet = TRUE)
  }

  msk_vrt_xml <- xml2::read_xml(mskvrt)
  msk_band <- xml2::xml_find_first(msk_vrt_xml, ".//VRTRasterBand")
  drop_nodatavalue(msk_band)
  xml2::xml_set_attr(msk_band, "subClass", "VRTDerivedRasterBand")
  xml2::xml_add_child(msk_band, "PixelFunctionType", "build_bitmask")
  xml2::xml_add_child(msk_band, "PixelFunctionLanguage", "Python")
  pf_args <- xml2::xml_add_child(msk_band, "PixelFunctionArguments")
  xml2::xml_set_attr(
    pf_args,
    "mask_values",
    paste(mask_values, collapse = ",")
  )

  # if (is.null(attr(build_mask_pixfun, "required_bands"))) {
  #   xml2::xml_set_attr(
  #     pf_args,
  #     "required_bands",
  #     paste(attr(build_mask_pixfun, "required_bands"), collapse = ",")
  #   )
  # }

  cdata_node <- xml2::xml_cdata(build_mask_pixfun)
  pixel_func_code <- xml2::xml_add_child(msk_band, "PixelFunctionCode")
  xml2::xml_add_child(pixel_func_code, cdata_node)

  wrp_msk_pf <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")
  xml2::write_xml(msk_vrt_xml, wrp_msk_pf)

  wmxmlsrc <- xml2::read_xml(as.character(
    xml2::xml_find_first(
      # we take the first value if mutliple - we just need a copy anyway.
      bands[[mask_idx[1]]],
      ".//SimpleSource | .//ComplexSource"
    )
  ))

  source_filename <- xml2::xml_find_first(wmxmlsrc, ".//SourceFilename")
  xml2::xml_set_text(source_filename, fs::path_file(wrp_msk_pf)) #
  sourceband <- xml2::xml_find_first(wmxmlsrc, ".//SourceBand")
  xml2::xml_remove(sourceband)
  xml2::xml_attr(source_filename, "relativeToVRT") <- "1"

  # update all other bands
  data_bands <- if (length(mask_idx) == 1) {
    bands[-mask_idx]
  } else {
    bands
  }

  purrr::walk(data_bands, function(.x) {
    xml2::xml_add_child(.x, wmxmlsrc)

    xml2::xml_set_attr(.x, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(.x, "PixelFunctionType", "bitmask")
    xml2::xml_add_child(.x, "PixelFunctionLanguage", "Python")
    pf_args <- xml2::xml_add_child(.x, "PixelFunctionArguments")
    xml2::xml_set_attr(
      pf_args,
      "valid_values",
      paste(mask_values, collapse = ",")
    )
    xml2::xml_set_attr(pf_args, "no_data_value", no_data)

    cdata_node <- xml2::xml_cdata(set_mask_pixfun)
    pixel_func_code <- xml2::xml_add_child(.x, "PixelFunctionCode")
    xml2::xml_add_child(pixel_func_code, cdata_node)
  })

  if (drop_mask_band && mask_band != "vrtility.mask.missing.mask") {
    xml2::xml_remove(bands[[mask_idx]])
  } else {
    if (mask_band != "vrtility.mask.missing.mask") {
      set_nodatavalue(bands[[mask_idx]], no_data)
    }
    #TODO: currently no way to add the omnicloudmask which would be nice.
  }

  # Write back to block
  tf <- fs::file_temp(tmp_dir = cache_dir, ext = "vrt")
  xml2::write_xml(vx, tf)

  tf <- set_vrt_metadata(
    tf,
    keys = "mask_band_name",
    values = mask_band,
    as_file = TRUE
  )

  build_vrt_block(
    tf,
    maskfun = build_mask_pixfun,
    pixfun = x$pixfun,
    warped = x$warped
  )
}

#' @param x A VRT collection
#' @rdname vrt_set_maskfun
#' @export
#' @return A VRT collection with the mask band set.
#' @export
vrt_set_maskfun.vrt_collection <- function(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = vrtility::build_intmask(),
  set_mask_pixfun = vrtility::set_mask_numpy(),
  drop_mask_band = TRUE,
  cache_dir = getOption("vrt.cache")
) {
  if (rlang::is_missing(mask_band)) {
    mask_band <- "vrtility.mask.missing.mask"
  }

  check_mask_band(x, mask_band, build_mask_pixfun)
  daemon_setup()

  masked_blocks <- purrr::map(
    x$vrt,
    purrr::in_parallel(
      function(.x) {
        vrtility::vrt_set_maskfun(
          .x,
          mask_band = mask_band,
          mask_values = mask_values,
          build_mask_pixfun = build_mask_pixfun,
          set_mask_pixfun = set_mask_pixfun,
          drop_mask_band = drop_mask_band,
          cache_dir = cache_dir
        )
      },
      mask_band = mask_band,
      mask_values = mask_values,
      build_mask_pixfun = build_mask_pixfun,
      set_mask_pixfun = set_mask_pixfun,
      drop_mask_band = drop_mask_band,
      cache_dir = getOption("vrt.cache")
    )
  )

  if (inherits(x, "vrt_collection_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }
  build_vrt_collection(
    masked_blocks,
    maskfun = build_mask_pixfun,
    pixfun = x$pixfun,
    warped = warped
  )
}


check_mask_band <- function(x, mb, bf) {
  if (!is.null(attr(bf, "mask_type"))) {
    return(invisible())
  }
  if (!mb %in% x$assets) {
    cli::cli_abort(
      c(
        "Could not find band named: {mb}",
        "i" = "Available bands: ",
        ">" = (paste(x$assets, collapse = ", "))
      )
    )
  }
}
