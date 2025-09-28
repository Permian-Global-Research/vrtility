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

  purrr::iwalk(jobs, function(j, i) {
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

  # "Pump" the event loop to process promise callbacks
  sleep_interval <- 0.001
  max_sleep_interval <- 0.1
  while (completed_count < length(jobs)) {
    later::run_now(0.01) # This processes promise callbacks!
    Sys.sleep(sleep_interval)
    sleep_interval <- min(sleep_interval * 2, max_sleep_interval)
  }

  close_datasets_safely(ds)

  return(invisible())
}


close_datasets_safely <- function(ds) {
  if (is.null(ds)) {
    return()
  }

  datasets <- if (inherits(ds, "list")) ds else list(ds)

  purrr::walk(datasets, function(d) {
    if (inherits(d, "GDALRaster")) {
      tryCatch(
        {
          if (d$isOpen()) d$close()
        },
        error = function(e) {
          cli::cli_warn("Failed to close dataset: {e$message}")
        }
      )
    }
  })
}
