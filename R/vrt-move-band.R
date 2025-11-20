#' @title Add an empty band to a VRT_x object
#' @param x A VRT_x object
#' @param band_idx numeric indicating the band number of the band to move.
#' @param after numeric indicating the band after which the new band should be
#' places. Note this is based on the initial state of the band ordering. eg. do
#' not add 1 if you are moving the band forward.
#' @export
vrt_move_band <- function(x, band_idx, after) {
  v_assert_type(after, "after", "numeric", nullok = FALSE)
  UseMethod("vrt_move_band")
}

#' @noRd
#' @keywords internal
#' @export
vrt_move_band.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_move_band method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
vrt_move_band.vrt_block <- function(
  x,
  band_idx,
  after
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

  if (band_idx < 1 || band_idx > length(x$assets) || band_idx == after) {
    cli::cli_abort(
      c(
        "x" = "For the provided vrt object, which has {length(x$assets)} band{?s},
      The `band_idx` argument must be between 1 and {length(x$assets)} and not equal to `after``."
      ),
      class = "vrtility_band_error"
    )
  }

  # Get the VRT XML
  vrt_xml <- xml2::read_xml(x$vrt)

  # Insert the new band after the specified position
  bands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  mv_band <- bands[[band_idx]]

  if (after == 0) {
    xml2::xml_add_sibling(bands[[1]], mv_band, .where = "before")
  } else {
    xml2::xml_add_sibling(bands[[after]], mv_band, .where = "after")
  }

  upbands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  old_position <- ifelse(band_idx < after, band_idx, band_idx + 1)

  xml2::xml_remove(upbands[[old_position]])

  newbands <- xml2::xml_find_all(vrt_xml, ".//VRTRasterBand")

  purrr::iwalk(
    newbands,
    ~ xml2::xml_set_attr(.x, "band", as.character(.y))
  )

  out_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

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
vrt_move_band.vrt_collection <- function(
  x,
  band_idx,
  after
) {
  block_list <- purrr::map(
    x[[1]],
    ~ vrt_move_band(
      .x,
      band_idx = band_idx,
      after = after
    )
  )
  build_vrt_collection(
    block_list,
    pixfun = x$pixfun,
    maskfun = x$maskfun,
    warped = x$warped
  )
}
