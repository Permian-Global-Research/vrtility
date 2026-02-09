#' Stack VRT files from a vrt_collection object
#' @param x A vrt_collection object
#' @details This function stacks VRT files from a vrt_collection object into a
#' single VRT file containing multiple layers for each RasterBand. The VRT
#' files are stacked in the order they are provided in the vrt_collection
#' object. If this is derived from a rstac object, the order should be ordered
#' by date.
#' @return A vrt_stack object
#' @export
#' @rdname vrt_stack
vrt_stack <- function(x) {
  UseMethod("vrt_stack")
}


#' @noRd
#' @export
vrt_stack.default <- function(x) {
  cli::cli_abort(
    "{cli::code_highlight('vrt_stack()')} not implemented for class {class(x)[1]}"
  )
}

#' @export
#' @rdname vrt_stack
vrt_stack.vrt_collection <- function(
  x
) {
  assert_srs_len(x)

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

  suppressWarnings(gdalraster::buildVRT(
    vrt_filename = main_vrt,
    input_rasters = vrt_paths,
    cl_arg = src_block_size(vrt_paths[1]),
    quiet = TRUE
  ))

  main_vrt <- set_vrt_descriptions(
    main_vrt,
    x$assets,
    as_file = TRUE
  )

  main_vrt <- set_vrt_metadata(
    main_vrt,
    keys = paste0("datetime_", seq_along(x$date_time)),
    values = x$date_time,
    as_file = TRUE
  )

  if (inherits(x, "vrt_collection_warped")) {
    warped <- TRUE
  } else {
    warped <- FALSE
  }

  build_vrt_stack(
    main_vrt,
    n_items = x$n_items,
    maskfun = x$maskfun,
    pixfun = x$pixfun,
    warped = warped
  )
}


#' constructor for vrt_block class
#' @param x A vrt_collection type object
#' @param maskfun The code of the mask function
#' @param pixfun The code of the pixel function
#' @param warped Logical. Is this a warped VRT?
#' @param n_items Number of items in the collection
#' @return A vrt_stack object
#' @keywords internal
#' @noRd
build_vrt_stack <- function(
  x,
  n_items,
  maskfun = NULL,
  pixfun = NULL,
  warped = FALSE
) {
  # validate the vrt against the schema
  v_assert_valid_schema(x)
  # read and verify modified VRT
  gdr <- methods::new(gdalraster::GDALRaster, x)
  on.exit(gdr$close(), add = TRUE)
  ras_count <- gdr$getRasterCount()
  assets <- purrr::map_chr(
    seq_len(ras_count),
    function(.x) gdr$getDescription(.x)
  )
  no_data_val <- purrr::map_dbl(
    seq_len(ras_count),
    function(.x) gdr$getNoDataValue(.x)
  )

  dttm <- purrr::map_chr(
    seq_len(n_items),
    ~ gdr$getMetadataItem(0, paste0("datetime_", .x), "")
  )

  rvrt <- list(
    vrt = as.character(xml2::read_xml(x)),
    vrt_src = x,
    srs = gdr$getProjection(),
    bbox = gdr$bbox(),
    res = gdr$res(),
    date_time = dttm,
    n_items = n_items,
    assets = assets,
    no_data_val = no_data_val,
    pixfun = pixfun,
    maskfun = maskfun,
    warped = warped
  )

  if (warped) {
    warped <- "vrt_stack_warped"
  } else {
    warped <- NULL
  }

  class(rvrt) <- c(warped, "vrt_stack", "vrt_block", "list")

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
