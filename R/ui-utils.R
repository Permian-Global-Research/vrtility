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
    .envir = rlang::caller_env()) {
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
