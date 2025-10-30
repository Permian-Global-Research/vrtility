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
  resolved <- logical(length(jobs))
  unresolved_idx <- seq_along(jobs)
  # Continue polling for results until all jobs are resolved
  while (any(!resolved)) {
    # Check all unresolved jobs in one pass
    any_resolved <- FALSE
    for (i in unresolved_idx) {
      if (mirai::unresolved(jobs[[i]])) {
        next
      }

      j <- mirai::collect_mirai(jobs[[i]])

      if (inherits(j, "miraiError")) {
        cli::cli_abort(
          "{msg}:  {j}",
          class = "mirai_async_error"
        )
      }

      rlang::eval_bare(expr)
      # If the job is resolved, mark it as such
      resolved[[i]] <- TRUE
      any_resolved <- TRUE
    }
    
    # Update unresolved indices only once per iteration
    unresolved_idx <- which(!resolved)
    
    # Only sleep if no jobs were resolved in this iteration to avoid busy-waiting
    if (!any_resolved && length(unresolved_idx) > 0) {
      Sys.sleep(0.01) # Sleep for 10ms instead of 1ms when no progress
    }
  }
  return(invisible())
}
