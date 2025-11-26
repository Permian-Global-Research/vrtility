#' constructor for vrt_block class
#' @param x A VRT file path
#' @param maskfun A function of the mask function
#' @param pixfun A function of the pixel function
#' @param warped A logical indicating whether the VRT is warped
#' @param is_remote A logical indicating whether the VRT is remote
#' @return A VRT block object
#' @keywords internal
#' @noRd
#' @export
build_vrt_block <- function(
  x,
  maskfun = NULL,
  pixfun = NULL,
  warped = FALSE,
  is_remote = FALSE
) {
  # validate the vrt against the schema
  v_assert_valid_schema(x)

  # read and verify and get attrs for modified VRT
  gdr <- methods::new(gdalraster::GDALRaster, x)
  on.exit(gdr$close(), add = TRUE)
  bt <- block_template(gdr)

  rvrt <- list(
    vrt = as.character(xml2::read_xml(x)),
    vrt_src = x,
    srs = gdr$getProjection(),
    bbox = gdr$bbox(),
    res = gdr$res(),
    date_time = bt$dttm,
    assets = bt$assets,
    no_data_val = bt$no_data_val,
    mask_band_name = bt$mask_band_name,
    pixfun = pixfun,
    maskfun = maskfun,
    warped = warped,
    is_remote = is_remote
  )

  class(rvrt) <- c("vrt_block", "list")

  return(rvrt)
}


block_template <- function(ds) {
  ras_count <- ds$getRasterCount()
  assets <- purrr::map_chr(
    seq_len(ras_count),
    function(.x) ds$getDescription(.x)
  )
  no_data_val <- purrr::map_dbl(
    seq_len(ras_count),
    function(.x) ds$getNoDataValue(.x)
  )
  # for stacks there will be multiple datetime_{n} keys
  metadata_items <- ds$getMetadata(0, "")
  datetime_keys <- metadata_items[grepl("^datetime_\\d+", metadata_items)]
  datetime_keys <- sub("=.*", "", datetime_keys)
  if (length(datetime_keys) == 0) {
    datetime_keys <- "datetime"
  }
  dttm <- purrr::map_chr(datetime_keys, ~ ds$getMetadataItem(0, .x, ""))

  # mask band name
  mask_band_name <- ds$getMetadataItem(0, "mask_band_name", "")
  list(
    assets = assets,
    no_data_val = no_data_val,
    dttm = dttm,
    mask_band_name = mask_band_name,
    dttm_keys = datetime_keys
  )
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
