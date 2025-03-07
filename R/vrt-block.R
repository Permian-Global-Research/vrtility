#' constructor for vrt_block class
#' @param xml An xml_document object
#' @param srs A character string of the spatial reference system
#' @param bbox A numeric vector of the bounding box
#' @param date_time A character string of the date time
#' @param assets A character vector of the asset names
#' @param no_data_val A numeric vector of the no data values
#' @param maskfun A function of the mask function
#' @param pixfun A function of the pixel function
#' @param ... Additional arguments not used
#' @keywords internal
#' @noRd
build_vrt_block <- function(
  x,
  maskfun = NULL,
  pixfun = NULL,
  ...
) {
  # validate the vrt against the schema
  v_assert_valid_schema(x)

  # read and verify and get attrs for modified VRT
  gdr <- new(gdalraster::GDALRaster, x)
  ras_count <- gdr$getRasterCount()
  assets <- purrr::map_chr(
    seq_len(ras_count),
    function(.x) gdr$getDescription(.x)
  )
  no_data_val <- purrr::map_dbl(
    seq_len(ras_count),
    function(.x) gdr$getNoDataValue(.x)
  )
  dttm <- gdr$getMetadataItem(0, "datetime", "")
  mask_band_name <- gdr$getMetadataItem(0, "mask_band_name", "")

  rvrt <- list(
    vrt = as.character(xml2::read_xml(x)),
    srs = gdr$getProjection(),
    bbox = gdr$bbox(),
    res = gdr$res(),
    date_time = dttm,
    assets = assets,
    no_data_val = no_data_val,
    mask_band_name = mask_band_name,
    pixfun = pixfun,
    maskfun = maskfun
  )

  class(rvrt) <- c("vrt_block", "list")

  return(rvrt)
}

#' @export
print.vrt_block <- function(x, xml = FALSE, pixfun = FALSE, ...) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_green("<VRT Block>"))))
  if (xml) {
    xml_printer(x$vrt)
  } else {
    vrt_print_msg()
  }
  if (!is.null(x$pixfun)) {
    if (pixfun) {
      pixel_fun_printer(x$pixfun)
    } else {
      pix_fun_print_msg()
    }
  }

  cli::cli_inform(
    c(
      crs_printer(x$srs),
      bbox_printer(x$bbox),
      res_printer(x$res),
      assets_printer(x$assets),
      no_data_printer(x$no_data_val),
      dttm_printer(x$date_time, "dttm")
    )
  )
  invisible(x)
}
