#' @title Mirai asynchronous result handler for windows
#' @description Handles the results of asynchronous mirai jobs, does not use
#' promises. using promises fails tests on windows...
#' @param jobs A list of mirai jobs.
#' @param ds The GDALRaster dataset to write to.
#' @param ... Additional arguments to bind in the current environment.
#' @param expr The expression to evaluate when a job is resolved.
#' @param msg The message to display in case of an error.
#' @return invisible
#' @keywords internal
#' @noRd
mirai_async_result_handler <- function(
  jobs,
  ds,
  ...,
  expr,
  msg = "mirai async error"
) {
  dots <- rlang::list2(...)
  rlang::env_bind(rlang::current_env(), !!!dots)

  jobs <- unclass(jobs)
  original_idx <- seq_along(jobs)

  while (length(jobs) > 0) {
    pos <- mirai::race_mirai(jobs)
    i <- original_idx[pos]

    j <- jobs[[pos]]$data

    if (inherits(j, "miraiError")) {
      cli::cli_abort(
        "{msg}:  {j}",
        class = "mirai_async_error"
      )
    }

    rlang::eval_bare(expr)

    jobs <- jobs[-pos]
    original_idx <- original_idx[-pos]
  }
  return(invisible())
}
