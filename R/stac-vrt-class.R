#' Print a stac_vrt object
#' @param x A stac_vrt object
#' @param xml A logical indicating whether to print the XML
#' @param ... Additional arguments not used
#' @export
#' @rdname stac_vrt
print.stac_vrt <- function(x, xml = FALSE, pixfun = FALSE, ...) {
  cli::cli_inform(c(">" = cli::style_bold(cli::col_green("STAC VRT"))))
  if (xml) {
    xml_printer(x$vrt)
  } else {
    cli::cli_inform(
      c(
        paste(cli::style_bold(
          cli::col_yellow("VRT XML: "),
          "[hidden]"
        )),
        " " = cli::style_italic(
          "run {cli::code_highlight('print(x, xml = TRUE)')} to view"
        )
      )
    )
  }
  if (!is.null(x$pixfun)) {
    if (pixfun) {
      cli::cli_inform(
        paste(cli::style_bold(cli::col_blue("Pixel Function:"))),
      )

      cli::code_highlight(
        cli::cli_code(format(x$pixfun), language = "python")
      )
    } else {
      cli::cli_inform(
        c(
          paste(cli::style_bold(
            cli::col_yellow("Pixel Function: "),
            "[hidden]"
          )),
          " " = cli::style_italic(
            "run {cli::code_highlight('print(x, pixfun = TRUE)')} to view"
          )
        )
      )
    }
  }


  cli::cli_inform(
    c(
      paste(
        cli::style_bold(cli::col_blue("Bounding Box:")),
        paste(x$bbox, collapse = " ")
      ),
      paste(cli::style_bold(cli::col_blue("Start Date:")), x$start_date),
      paste(cli::style_bold(cli::col_blue("End Date:")), x$end_date),
      paste(cli::style_bold(cli::col_blue("Number of Items:")), x$n_items),
      paste(
        cli::style_bold(cli::col_blue("Assets:")),
        paste(x$assets, collapse = ", ")
      )
    )
  )
  invisible(x)
}

xml_printer <- function(x) {
  cli::style_bold("VRT XML: \n\n") |>
    cli::col_yellow() |>
    cat()
  paste0(x, "\n") |>
    cli::style_italic() |>
    cli::col_green() |>
    cat()
}


#' Save a stac_vrt object to disk
#' @param x A stac_vrt object
#' @export
#' @rdname stac_vrt
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
#' @rdname stac_vrt
save_vrt.stac_vrt <- function(
    x,
    outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "vrt")) {
  vrt_xml <- xml2::read_xml(x$vrt)
  xml2::write_xml(vrt_xml, outfile)
  return(outfile)
}

#' constructor for stac_vrt class
#' @param xml An xml_document object
#' @param bbox A numeric vector of the bounding box
#' @param start_date A character string of the start date
#' @param end_date A character string of the end date
#' @param n_its An integer of the number of items
#' @param assets A character vector of the asset names
#' @param pixfun A function of the pixel function
#' @keywords internal
#' @noRd
build_stac_vrt <- function(
    xml, bbox, start_date, end_date, n_its, assets, pixfun = NULL) {
  rvrt <- list(
    vrt = as.character(xml),
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    n_items = n_its,
    assets = assets,
    pixfun = pixfun
  )

  class(rvrt) <- "stac_vrt"

  return(rvrt)
}
