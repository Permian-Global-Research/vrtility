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
  srs,
  bbox,
  start_date,
  end_date,
  n_its,
  assets,
  maskfun = NULL,
  pixfun = NULL,
  ...
) {
  rvrt <- list(
    vrt = as.character(x),
    srs = srs,
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    n_items = n_its,
    assets = assets,
    pixfun = pixfun
  )

  class(rvrt) <- c("vrt_stack", "vrt_block", "list")

  return(rvrt)
}

#' Print a vrt_block object
#' @param x A vrt_block object
#' @param xml A logical indicating whether to print the XML
#' @param ... Additional arguments not used
#' @export
#' @rdname vrt_block
print.vrt_stack <- function(x, xml = FALSE, pixfun = FALSE, ...) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_green("STAC VRT"))))
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
      bbox_printer(x$bbox),
      dttm_printer(x, "start"),
      dttm_printer(x, "end"),
      n_items_printer(x$n_items),
      assets_printer(x$assets)
    )
  )
  invisible(x)
}


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
  srs,
  bbox,
  res,
  date_time,
  assets,
  no_data_val,
  maskfun = NULL,
  pixfun = NULL,
  ...
) {
  rvrt <- list(
    vrt = as.character(x),
    srs = srs,
    bbox = bbox,
    res = res,
    date_time = date_time,
    assets = assets,
    no_data_val = no_data_val,
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


#' @keywords internal
#' @noRd
#' @export
build_vrt_collection <- function(
  x,
  srs,
  bbox,
  res,
  start_date,
  end_date,
  n_its,
  assets,
  maskfun = NULL,
  pixfun = NULL,
  ...
) {
  rvrt <- list(
    vrt = x,
    srs = srs,
    bbox = bbox,
    res = res,
    start_date = start_date,
    end_date = end_date,
    n_items = n_its,
    assets = assets,
    pixfun = pixfun
  )

  class(rvrt) <- c("vrt_collection", "vrt_block", "list")

  return(rvrt)
}


#' @export
#' @rdname vrt_block
print.vrt_collection <- function(
  x,
  xml = FALSE,
  pixfun = FALSE,
  blocks = FALSE,
  ...
) {
  if (blocks) {
    purrr::walk(x, print)
    return(invisible(x))
  }
  cli::cli_inform(c(
    ">" = cli::style_bold(cli::col_green("<VRT Collection> \n"))
  ))

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
      dttm_printer(x$start_date, "start"),
      dttm_printer(x$end_date, "end"),
      n_items_printer(x$n_items),
      assets_printer(x$assets)
    )
  )
  invisible(x)
}


#' Save a vrt_block object to disk
#' @param x A vrt_block object
#' @export
#' @rdname vrt_block
save_vrt <- function(x, file) {
  UseMethod("save_vrt")
}

#' @keywords internal
#' @noRd
save_vrt.default <- function(x, file) {
  cli::cli_abort(
    "The save_vrt method is not implemented for class {class(x)}",
    class = "vrtility_type_error"
  )
}

#' @export
#' @rdname vrt_block
save_vrt.vrt_stack <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
) {
  vrt_xml <- xml2::read_xml(x$vrt)
  xml2::write_xml(vrt_xml, outfile)
  return(outfile)
}
