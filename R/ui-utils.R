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
    cli::cli_abort("Quitting...", call = NULL)
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

pixel_fun_printer <- function(x, type = c("pf", "mf")) {
  type <- rlang::arg_match(type, c("pf", "mf"))
  type <- switch(type, pf = "Pixel Function", mf = "Mask Function")

  cli::cli_inform(
    paste(cli::style_bold(cli::col_blue("{type}:"))),
  )

  cli::code_highlight(
    cli::cli_code(format(x), language = "python")
  )
  cat("\n")
}

pix_fun_print_msg <- function(type = c("pf", "mf")) {
  type <- rlang::arg_match(type, c("pf", "mf"))
  tname <- switch(type, pf = "Pixel Function", mf = "Mask Function")
  arg_name <- switch(
    type,
    pf = "run {cli::code_highlight('print(x, pixfun = TRUE)')} to view",
    mf = "run {cli::code_highlight('print(x, maskfun = TRUE)')} to view"
  )

  cli::cli_inform(
    c(
      paste(cli::style_bold(
        cli::col_yellow("{tname}: "),
        "[hidden]"
      )),
      " " = cli::style_italic(
        arg_name
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

  if (!inherits(x, "POSIXct")) {
    x <- lubridate::as_datetime(x)
  }

  x <- switch(type, dttm = x, start = min(x), end = max(x))

  paste(
    cli::style_bold(cli::col_blue(
      glue::glue(dt_lab),
      ":"
    )),
    strftime(x, tz = lubridate::tz(x), usetz = TRUE)
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
  cli::style_bold("\n VRT SRS: \n") |>
    cli::col_yellow() |>
    cat()
  paste0(x, "\n", sep = "\n") |>
    cli::style_italic() |>
    cli::col_br_magenta() |>
    cat()
}


opts_check <- function(x, o) {
  if (o %in% x) {
    cli::cli_warn(
      "The {o} option is already set in the warp options"
    )
  }
}


fastnanquantile_msg <- function() {
  cli::rule(
    center = "NOTE",
    line_col = "#64b1f0"
  )

  cli::cli_inform(
    c(
      "i" = "Using 'fastnanquantile' for quantile calculation",
      "!" = "When using {cli::code_highlight('vrt_compute()')}, 
      it is strongly advised to:",
      "*" = "use the {cli::style_bold('warp')} engine",
      "*" = "use sequential processing ({cli::style_bold(\"DONT'T\")} use 
      mirai daemons)"
    )
  )

  cli::rule(
    line_col = "#64b1f0"
  )
}
