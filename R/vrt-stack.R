#' Stack VRT files from a vrt_collection object
#' @param x A vrt_collection object
#' @param ... Additional arguments passed to methods
#' @details This function stacks VRT files from a vrt_collection object into a
#' single VRT file containing multiple layers for each RasterBand. The VRT
#' files are stacked in the order they are provided in the vrt_collection
#' object. If this is derived from a rstac object, the order should be ordered by date.
#' @return A vrt_stack object
#' @export
#' @rdname vrt_stack
vrt_stack <- function(x, ...) {
  UseMethod("vrt_stack")
}


#' @noRd
#' @export
vrt_stack.default <- function(x, ...) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_stack()')} not implemented for class {class(x)[1]}"
  )
}

#' @export
#' @param quiet Logical. If TRUE, suppress GDAL progress bar
#' @rdname vrt_stack
vrt_stack.vrt_collection <- function(x, quiet = TRUE, ...) {
  vrt_paths <- purrr::map_chr(
    x[[1]],
    function(.x) {
      tf <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
      vrt <- xml2::read_xml(.x$vrt)
      xml2::write_xml(vrt, tf)
      return(tf)
    }
  )

  main_vrt <- fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")

  gdalraster::buildVRT(
    vrt_filename = main_vrt,
    input_rasters = vrt_paths,
    cl_arg = c("-allow_projection_difference"), # TODO: is this still  needed with warped VRT?
    quiet = quiet
  )

  main_vrt <- set_vrt_descriptions(
    main_vrt,
    x$assets,
    as_file = TRUE
  ) #|>
  main_vrt <- set_vrt_metadata(
    main_vrt,
    keys = paste0("datetime_", seq_along(x$date_time)),
    values = x$date_time,
    as_file = TRUE
  )

  build_vrt_stack(main_vrt, pix_fun = x$pixfun, maskfun = x$maskfun)
}


#' constructor for vrt_block class
#' @param xml An xml_document object
#' @param bbox A numeric vector of the bounding box
#' @param start_date A character string of the start date
#' @param end_date A character string of the end date
#' @param n_its An integer of the number of items
#' @param assets A character vector of the asset names
#' @param pixfun A function of the pixel function
#' @keywords internal
#' @noRd
build_vrt_stack <- function(
  x,
  maskfun = NULL,
  pixfun = NULL,
  ...
) {
  # validate the vrt against the schema
  v_assert_valid_schema(x)
  # read and verify modified VRT
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
  nits <- length(setdiff(gdr$getFileList(), gdr$getFilename()))
  dttm <- purrr::map_chr(
    seq_len(nits),
    ~ gdr$getMetadataItem(0, paste0("datetime_", .x), "")
  )

  rvrt <- list(
    vrt = as.character(xml2::read_xml(x)),
    srs = gdr$getProjection(),
    bbox = gdr$bbox(),
    res = gdr$res(),
    date_time = dttm,
    n_items = nits,
    assets = assets,
    no_data_val = no_data_val,
    pixfun = pixfun,
    maskfun = maskfun
  )

  class(rvrt) <- c("vrt_stack", "vrt_block", "list")

  return(rvrt)
}

#' Print a vrt_block object
#' @param xml A logical indicating whether to print the XML
#' @param pixfun A logical indicating whether to print the pixel function
#' @param maskfun A logical indicating whether to print the mask function
#' @param ... Additional arguments not used
#' @export
#' @rdname vrt_stack
print.vrt_stack <- function(
  x,
  xml = FALSE,
  pixfun = FALSE,
  maskfun = FALSE,
  ...
) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_green("VRT STACK"))))
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

  if (!is.null(x$maskfun)) {
    if (maskfun) {
      pixel_fun_printer(x$maskfun, type = "mf")
    } else {
      pix_fun_print_msg(type = "mf")
    }
  }

  cli::cli_inform(
    c(
      crs_printer(x$srs),
      bbox_printer(x$bbox),
      dttm_printer(x$date_time, "start"),
      dttm_printer(x$date_time, "end"),
      n_items_printer(x$n_items),
      assets_printer(x$assets)
    )
  )
  invisible(x)
}


#' Save a vrt_block object to disk
#' @export
#' @rdname vrt_stack
save_vrt <- function(x, outfile) {
  UseMethod("save_vrt")
}

#' @keywords internal
#' @noRd
save_vrt.default <- function(x, ...) {
  cli::cli_abort(
    "The save_vrt method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
#' @param outfile A character string of the output file
#' @rdname vrt_stack
save_vrt.vrt_block <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
) {
  vrt_xml <- xml2::read_xml(x$vrt)
  xml2::write_xml(vrt_xml, outfile)
  return(outfile)
}
