#' @title Add an empty band to a VRT_x object
#' @param after numeric indicating the band number to add the empty band after
#' @param x A VRT_x object
#' @param description A character string describing the empty band
#' @param save_dir A character string indicating the directory to save the VRT -
#' typically not required mainly here for manaing temp file locs in parallel.
#' @param scale_value A numeric value to set the scale of the new band. If NULL,
#' the scale of the first band in the VRT will be used. Be careful, Landsat for
#' example has different scales for different bands.ddoc
#'
#' @export
vrt_add_empty_band <- function(x, after, description, save_dir, scale_value) {
  v_assert_type(after, "after", "numeric", nullok = FALSE)
  UseMethod("vrt_add_empty_band")
}

#' @noRd
#' @keywords internal
#' @export
vrt_add_empty_band.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_add_empty_band method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
vrt_add_empty_band.vrt_block <- function(
  x,
  after,
  description,
  save_dir = getOption("vrt.cache"),
  scale_value = NULL
) {
  if (after < 0 || after > length(x$assets) + 1) {
    cli::cli_abort(
      c(
        "x" = "For the provided vrt object, which has {length(x$assets)} band{?s},
      The `after` argument must be between 0 and {length(x$assets) + 1}."
      ),
      class = "vrtility_after_error"
    )
  }

  tvrt <- vrt_save(x)

  inblock <- methods::new(
    gdalraster::GDALRaster,
    tvrt,
    read_only = TRUE
  )
  on.exit(inblock$close(), add = TRUE)

  empty_band_src <- suppressMessages(gdalraster::rasterFromRaster(
    tvrt,
    fs::file_temp(
      tmp_dir = save_dir,
      ext = "tif"
    ),
    fmt = "GTiff",
    nbands = 1,
    dtName = inblock$getDataTypeName(1),
    options = c(
      "COMPRESS=ZSTD",
      "ZSTD_LEVEL=1",
      "PREDICTOR=2",
      "TILED=YES",
      glue::glue(
        "BLOCKXSIZE={inblock$getBlockSize(1)[1]}"
      ),
      glue::glue(
        "BLOCKYSIZE={inblock$getBlockSize(1)[2]}"
      ),
      "BIGTIFF=IF_NEEDED"
    ),
    init = inblock$getNoDataValue(1)
  ))

  eb_vrt <- fs::file_temp(tmp_dir = save_dir, ext = "vrt")
  gdalraster::buildVRT(eb_vrt, empty_band_src, quiet = TRUE)

  ebxml <- xml2::read_xml(eb_vrt)
  ebband <- xml2::xml_find_first(ebxml, ".//VRTRasterBand")
  xml2::xml_add_child(ebband, "Description", description)

  # Get the VRT XML
  vrt_xml <- xml2::read_xml(x$vrt)

  # Insert the new band after the specified position
  bands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  # resolve any scaling values
  if (is.null(scale_value)) {
    scale_vals <- xml2::xml_find_all(vrt_xml, ".//Scale")
    scale_value <- purrr::map_chr(
      scale_vals,
      ~ xml2::xml_text(.x)
    )

    scale_value <- names(sort(table(scale_value), decreasing = TRUE)[1])
  }
  # apply scale to new band (enables combining with other scaled collections)
  xml2::xml_add_child(ebband, "Scale", as.character(scale_value))

  if (after == 0) {
    xml2::xml_add_sibling(bands[[1]], ebband, .where = "before")
  } else {
    xml2::xml_add_sibling(bands[[after]], ebband, .where = "after")
  }

  bands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  purrr::iwalk(
    bands,
    ~ xml2::xml_set_attr(.x, "band", as.character(.y))
  )

  out_vrt <- fs::file_temp(tmp_dir = save_dir, ext = "vrt")

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
vrt_add_empty_band.vrt_collection <- function(
  x,
  after,
  description,
  save_dir = getOption("vrt.cache"),
  scale_value = NULL
) {
  block_list <- purrr::map(
    x[[1]],
    ~ vrt_add_empty_band(
      .x,
      after = after,
      description = description,
      save_dir = save_dir,
      scale_value = scale_value
    )
  )
  build_vrt_collection(
    block_list,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = x$warped
  )
}
