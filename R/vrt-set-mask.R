#' Set mask band of a VRT collection
#' @param x A VRT type object (collection or block)
#' @param mask_values A numeric vector of integer or bit values to be masked.
#' @param mask_band The name of the mask band
#' @param build_mask_pixfun A character string of the Python code or muparser
#' expression to build the mask. If `NULL` (default), automatically uses
#' [build_intmask()] which will choose muparser if the option `vrtility.use_muparser`
#' is `TRUE` with no buffering, otherwise a Python based implementation is used.
#'  Provided functions include \code{build_intmask} and \code{build_bitmask}. See details.
#' @param buffer_size A buffer size to apply to the mask (numeric, default: 0). A buffer
#' size > 0 will dilate the mask by the specified number of pixels.
#' This can be useful to remove edge effects around clouds.
#' If a buffer size > 0 is specified, the `scipy` python library will
#' automatically be installed and Python will be used (muparser cannot do buffering).
#' @param drop_mask_band Logical. If TRUE, the mask band will be removed from
#' the VRT block.
#' @export
#' @rdname vrt_set_maskfun
#' @details
#' The `build_mask_pixfun` function is used to build the mask band. Where the
#' mask band is a true bitmask and bit-wise operations are required, the
#' [build_bitmask()] function should be used. For integer-based masking, where
#' the mask band is provided as a single band with integer values, the
#' [build_intmask()] function should be used.
#'
#' By default (when `build_mask_pixfun = NULL`), the function automatically
#' selects the most efficient implementation:
#' - GDAL >= 3.12 with no buffering: Uses muparser expressions (fastest, no Python)
#' - GDAL < 3.12 or buffering needed: Uses Python/NumPy
#'
#'
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#'
#' ex_collect <- vrt_collect(s2files)
#'
#' # Auto-selects muparser or Python based on GDAL version
#' ex_collect |>
#'   vrt_set_maskfun(
#'     mask_band = "SCL",
#'     mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
#'     drop_mask_band = FALSE)
#'
#' # Force Python implementation
#' ex_collect |>
#'   vrt_set_maskfun(
#'     mask_band = "SCL",
#'     mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
#'     build_mask_pixfun = build_intmask(),
#'     drop_mask_band = FALSE)
#'
vrt_set_maskfun <- function(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
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
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
) {
  v_assert_type(mask_band, "mask_band", "character", nullok = FALSE)
  v_assert_type(
    mask_values,
    "mask_values",
    c("numeric", "integer"),
    nullok = FALSE,
    multiple = TRUE
  )

  # Auto-detect build_mask_pixfun if not provided
  if (is.null(build_mask_pixfun)) {
    build_mask_pixfun <- build_intmask()
  }

  v_assert_type(build_mask_pixfun, "mask_pix_fun", "character", nullok = FALSE)

  set_mask_pixfun <- set_mask(
    buffer_size = buffer_size
  )

  vx <- xml2::read_xml(x$vrt)

  no_data <- xml2::xml_find_first(vx, ".//NoDataValue") |>
    xml2::xml_double()

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

  msk_vrt_xml <- vrt_subset_bands(
    x$vrt_src,
    mask_idx,
    return_type = "xml"
  )

  msk_band <- xml2::xml_find_first(msk_vrt_xml, ".//VRTRasterBand")
  drop_nodatavalue(msk_band)

  # Handle muparser expressions differently
  if (inherits(build_mask_pixfun, "muparser_expression")) {
    # For muparser, evaluate the template with mask_values
    expr_evaluated <- glue::glue(
      build_mask_pixfun
    )
    set_gdal_pixfun_xml(
      msk_band,
      "expression",
      list(
        expression = expr_evaluated,
        propagateNoData = "false"
      )
    )
  } else {
    # For Python, set up the pixel function
    xml2::xml_set_attr(msk_band, "subClass", "VRTDerivedRasterBand")
    xml2::xml_add_child(msk_band, "PixelFunctionType", "build_mask")
    xml2::xml_add_child(msk_band, "PixelFunctionLanguage", "Python")
    pf_args <- xml2::xml_add_child(msk_band, "PixelFunctionArguments")
    xml2::xml_set_attr(
      pf_args,
      "mask_values",
      paste(mask_values, collapse = ",")
    )

    cdata_node <- xml2::xml_cdata(build_mask_pixfun)
    pixel_func_code <- xml2::xml_add_child(msk_band, "PixelFunctionCode")
    xml2::xml_add_child(pixel_func_code, cdata_node)
  }

  wrp_msk_pf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(msk_vrt_xml, wrp_msk_pf)

  wmxmlsrc <- xml2::read_xml(as.character(
    vrt_find_first_src(bands[[mask_idx[1]]])
  ))

  source_filename <- xml2::xml_find_first(wmxmlsrc, ".//SourceFilename")
  xml2::xml_set_text(source_filename, fs::path_file(wrp_msk_pf))
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

    if (inherits(set_mask_pixfun, "muparser_expression")) {
      bands <- xml2::xml_attr(vrt_find_all_srcs(.x), "name") #nolint # bands is referenced in set_mask_muparser

      set_gdal_pixfun_xml(
        .x,
        "expression",
        list(
          expression = glue::glue(set_mask_pixfun),
          propagateNoData = "true"
        )
      )
    } else {
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
    }
  })

  if (drop_mask_band) {
    xml2::xml_remove(bands[[mask_idx]])
    # this next bit is needed incase the mask is not the last band.
    regrab_bands <- xml2::xml_find_all(vx, ".//VRTRasterBand")
    purrr::walk(seq_along(regrab_bands), function(i) {
      xml2::xml_set_attr(regrab_bands[[i]], "band", i)
    })
  } else {
    set_nodatavalue(bands[[mask_idx]], no_data)
  }

  # Write back to block
  tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
  xml2::write_xml(vx, tf)

  tf <- set_vrt_metadata(
    tf,
    keys = "mask_band_name",
    values = mask_band,
    as_file = TRUE
  )

  build_vrt_block(
    vrt_to_vrt(tf), #REVRT
    maskfun = build_mask_pixfun,
    pixfun = x$pixfun,
    warped = x$warped,
    is_remote = x$is_remote
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
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
) {
  check_mask_band(x, mask_band)
  daemon_setup()

  # Auto-detect build_mask_pixfun if not provided
  if (is.null(build_mask_pixfun)) {
    build_mask_pixfun <- build_intmask()
  }

  masked_blocks <- purrr::map(
    x$vrt,
    purrr::in_parallel(
      function(.x) {
        vrt_set_maskfun(
          .x,
          mask_band = mask_band,
          mask_values = mask_values,
          build_mask_pixfun = build_mask_pixfun,
          buffer_size = buffer_size,
          drop_mask_band = drop_mask_band
        )
      },
      vrt_set_maskfun = vrt_set_maskfun,
      mask_band = mask_band,
      mask_values = mask_values,
      build_mask_pixfun = build_mask_pixfun,
      buffer_size = buffer_size,
      drop_mask_band = drop_mask_band,
      src_block_size = src_block_size
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


check_mask_band <- function(x, mb) {
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
