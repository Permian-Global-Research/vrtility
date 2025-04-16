#' check if there are any running daemons
#' @noRd
#' @keywords internal
using_daemons <- function() {
  mirai::status()$connections > 0
}

#' check the number of daemons
#' @return number of daemons
#' @noRd
#' @keywords internal
n_daemons <- function() {
  mirai::status()$connections
}

#' function for managing the setting of the outer mirai daemons responsible for
#' managing the parallel processing across raster bands.
#' @param n the number daemons to set
#' @return invisible
#' @noRd
#' @keywords internal
#' @details This does not deal with internal chunk parallelism which is managed
#' by the `map_bands_and_chunks` function depending on the number of splits.
set_outer_daemons <- function(
  n,
  msg = c(
    "!" = "Active mirai daemons have been detected, but fewer than the number of
      bands.",
    "i" = "No changes were made to this mirai configuartion but this could
        result in performance issues"
  )
) {
  n_cons <- mirai::status()$connections
  if (n_cons == 0) {
    mirai::daemons(n)
  } else if (n_cons < n) {
    cli::cli_inform(
      msg
    )
  }
  invisible(n_cons)
}
