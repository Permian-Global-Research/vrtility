#' @title Mirai asynchronous result handler
#' @description Handles the results of asynchronous mirai jobs, checking for
#' unresolved values and evaluating an expression when resolved.
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

  completed_count <- 0

  promise_list <- purrr::imap(jobs, function(j, i) {
    j |>
      promises::then(
        onFulfilled = function(j_result) {
          result <- rlang::eval_bare(
            expr,
            env = rlang::env(
              rlang::current_env(),
              i = i,
              j = j_result
            )
          )
          completed_count <<- completed_count + 1
          return(result)
        },
        onRejected = function(e) {
          completed_count <<- completed_count + 1
          cli::cli_abort("{msg}: {e}", class = "mirai_async_error")
        }
      ) |>
      promises::catch(function(e) {
        completed_count <<- completed_count + 1
        cli::cli_abort("{msg}: {e}", class = "mirai_async_error")
      })
  })

  if (length(promise_list) > 1) {
    promise_list <- promises::promise_all(.list = promise_list)
  } else {
    promise_list <- promise_list[[1]]
  }

  promise_list |>
    promises::then(function() {
      invisible()
    }) |>
    promises::finally(
      \(p) {
        if (length(ds) == 1) {
          ds$close()
        } else {
          purrr::walk(ds, \(d) d$close())
        }
      }
    )

  # "Pump" the event loop to process promise callbacks
  while (completed_count < length(jobs)) {
    later::run_now(0.01) # This processes promise callbacks!
    Sys.sleep(0.001) # Small sleep to prevent busy waiting
  }

  return(invisible())
}
