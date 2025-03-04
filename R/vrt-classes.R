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
#' @rdname vrt_classes
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
  dttm <- gdr$getMetadataItem(0, "datetime", "")

  rvrt <- list(
    vrt = as.character(xml2::read_xml(x)),
    srs = gdr$getProjection(),
    bbox = gdr$bbox(),
    res = gdr$res(),
    date_time = dttm,
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
build_vrt_collection <- function(
  x,
  pixfun = NULL,
  maskfun = NULL,
  ...
) {
  bbox <- purrr::map(x, function(.x) .x$bbox) |>
    purrr::reduce(
      function(.x, .y) {
        c(
          min(.x[1], .y[1]),
          min(.x[2], .y[2]),
          max(.x[3], .y[3]),
          max(.x[4], .y[4])
        )
      }
    )

  sd <- purrr::map_chr(
    x,
    function(.x) .x$date_time
  )

  uniq_assets <- purrr::map(
    x,
    function(.x) .x$assets
  ) |>
    unlist() |>
    unique()

  uniq_crs <- purrr::map(
    x,
    function(.x) .x$srs
  ) |>
    unlist() |>
    unique()

  min_res <- purrr::map(
    x,
    function(.x) .x$res
  ) |>
    purrr::reduce(
      function(.x, .y) {
        c(
          min(.x[1], .y[1]),
          min(.x[2], .y[2])
        )
      }
    )

  rvrt <- list(
    vrt = x,
    srs = uniq_crs,
    bbox = bbox,
    res = min_res,
    date_time = sd,
    n_items = length(x),
    assets = uniq_assets,
    pixfun = pixfun,
    maskfun = maskfun
  )

  class(rvrt) <- c("vrt_collection", "vrt_block", "list")

  return(rvrt)
}


#' @export
#' @param blocks A logical indicating whether to print the blocks instead of
#' the collection summary.
#' @param ... Additional arguments not used
#' @rdname vrt_classes
print.vrt_collection <- function(
  x,
  xml = FALSE,
  pixfun = FALSE,
  maskfun = FALSE,
  blocks = FALSE,
  ...
) {
  if (blocks) {
    print(x[[1]])
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
      res_printer(x$res),
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
#' @rdname vrt_classes
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
#' @rdname vrt_classes
save_vrt.vrt_stack <- function(
  x,
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")
) {
  vrt_xml <- xml2::read_xml(x$vrt)
  xml2::write_xml(vrt_xml, outfile)
  return(outfile)
}
