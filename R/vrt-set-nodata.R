#' @title Set NoData Value for VRT
#' @description Set the NoData value for a VRT_x object. This information is
#' preserved in derived VRT and GeoTIFF files.
#' @param x A VRT_x object
#' @param nodatavalue A numeric value to set the NoDataValue XML tag for the VRT_x
#' object.
#' @param nodata A numeric value to set the NODATA XML tag for the VRT_x
#' object. defaults to the same as `nodatavalue`.
#' @examples
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' ex_collect <- vrt_collect(s2files)
#' ex_nodata <- vrt_set_nodata(
#'   ex_collect,
#'   nodatavalue = 0,
#'   nodata = 0
#' )
#' print(ex_nodata[[1]][[1]], xml = TRUE)
#' @export
vrt_set_nodata <- function(x, nodatavalue, nodata, band_idx) {
  UseMethod("vrt_set_nodata")
}

#' @noRd
#' @keywords internal
#' @export
vrt_set_nodata.default <- function(x, ...) {
  cli::cli_abort(
    "The vrt_set_nodata method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
#' @rdname vrt_set_nodata
vrt_set_nodata.vrt_block <- function(
  x,
  nodatavalue,
  nodata = nodatavalue,
  band_idx = NULL
) {
  v_assert_type(nodatavalue, "nodatavalue", "numeric", nullok = FALSE)
  v_assert_type(nodata, "nodata", "numeric", nullok = FALSE)
  v_assert_type(
    band_idx,
    "band_idx",
    c("numeric", "integer"),
    nullok = TRUE,
    multiple = TRUE
  )

  purrr::pwalk(
    list(
      v = list(nodatavalue, nodata),
      n = list("nodatavalue", "nodata")
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
    list(band = bands[band_idx], nodatavalue = nodatavalue, nodata = nodata),
    function(band, nodatavalue, nodata) {
      # Remove existing NoDataValue elements
      reset_element(band, "NoDataValue", nodatavalue)

      band_src <- xml2::xml_find_first(band, ".//ComplexSource")
      # Remove existing NODATA elements
      reset_element(band_src, "NODATA", nodata)
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
#' @rdname vrt_set_nodata
vrt_set_nodata.vrt_collection <- function(
  x,
  nodatavalue,
  nodata = nodatavalue,
  band_idx = NULL
) {
  block_list <- purrr::map(
    x[[1]],
    ~ vrt_set_nodata(
      .x,
      nodatavalue = nodatavalue,
      nodata = nodata,
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
