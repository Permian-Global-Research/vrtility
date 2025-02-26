#' A menu for making choices in interactive sessions
#' @param prompt The prompt to display before the menu
#' @param choices A character vector of choices
#' @param not_interactive The message to display if the session is not
#' interactive
#' @param quit A numeric vector of choices that will quit the menu
#' @param .envir The environment in which to evaluate the function
#' @return An integer representing the selected choice
#' @keywords internal
#' @noRd
#' @details by hadely: https://github.com/r-lib/cli/issues/228
nice_menu <- function(
  prompt,
  choices,
  not_interactive = "This function requires an interactive session",
  quit = integer(),
  .envir = rlang::caller_env()
) {
  if (!rlang::is_interactive()) {
    cli::cli_abort(not_interactive, .envir = .envir)
  }

  choices <- paste0(seq_along(choices), " ", choices)
  cli::cli_inform(
    c(
      ">" = prompt,
      " " = "Select an option:",
      choices
    ),
    .envir = .envir
  )

  repeat {
    selected <- readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_inform("Enter an item from the menu, or 0 to exit")
  }

  selected <- as.integer(selected)
  if (selected %in% c(0, quit)) {
    cli::cli_abort("Quiting...", call = NULL)
  }
  selected
}


# printing helpers

vrt_print_msg <- function() {
  cli::cli_inform(
    c(
      paste(cli::style_bold(
        cli::col_yellow("\n VRT XML: "),
        "[hidden]"
      )),
      " " = cli::style_italic(
        "run {cli::code_highlight('print(x, xml = TRUE)')} to view"
      )
    )
  )
}

pixel_fun_printer <- function(x) {
  cli::cli_inform(
    paste(cli::style_bold(cli::col_blue("Pixel Function:"))),
  )

  cli::code_highlight(
    cli::cli_code(format(x), language = "python")
  )
}

pix_fun_print_msg <- function() {
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

bbox_printer <- function(x) {
  formatted_bbox <- format(x, scientific = FALSE, trim = TRUE)

  paste(
    cli::style_bold(cli::col_blue("Bounding Box:")),
    paste(formatted_bbox, collapse = " ")
  )
}

dttm_printer <- function(x, type = c("dttm", "start", "end")) {
  dt_lab <- switch(
    type,
    dttm = "Date Time",
    start = "Start Date",
    end = "End Date"
  )

  paste(
    cli::style_bold(cli::col_blue(
      glue::glue(dt_lab),
      ":"
    )),

    if (inherits(x, "POSIXct")) {
      x
    } else {
      lubridate::as_datetime(x)
    }
  )
}

n_items_printer <- function(x) {
  paste(
    cli::style_bold(cli::col_blue("Number of Items:")),
    x
  )
}

assets_printer <- function(x) {
  paste(
    cli::style_bold(cli::col_blue("Assets:")),
    paste(x, collapse = ", ")
  )
}

no_data_printer <- function(x) {
  paste(
    cli::style_bold(cli::col_blue("No Data Value(s):")),
    paste(x, collapse = ", ")
  )
}

res_printer <- function(x) {
  paste(
    cli::style_bold(cli::col_blue("Pixel res:")),
    paste(x, collapse = ", ")
  )
}


xml_printer <- function(x) {
  cli::style_bold("VRT XML: \n\n") |>
    cli::col_yellow() |>
    cat()
  paste0(x, "\n\n") |>
    cli::style_italic() |>
    cli::col_green() |>
    cat()
}


crs_printer <- function(x) {
  #TODO: add srs_find_epsg and friends when I get the dev gdalraster installed.
  cli::style_bold("\n VRT SRS: \n") |>
    cli::col_yellow() |>
    cat()
  paste0(x, "\n", sep = "\n") |>
    cli::style_italic() |>
    cli::col_br_magenta() |>
    cat()
}
