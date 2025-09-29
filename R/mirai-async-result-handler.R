#' @title Mirai asynchronous result handler for unix
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
mirai_async_result_handler_unix <- function(
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

  close_datasets_safely(promise_list, ds)

  # "Pump" the event loop to process promise callbacks
  run_now_dur <- 0.01
  sleep_interval <- 0.001
  max_sleep_interval <- 0.1
  while (completed_count < length(jobs)) {
    later::run_now(run_now_dur) # This processes promise callbacks!
    Sys.sleep(sleep_interval)
    sleep_interval <- min(sleep_interval * 2, max_sleep_interval)
  }

  return(invisible())
}


close_datasets_safely <- function(plist, ds) {
  datasets <- if (inherits(ds, "list")) ds else list(ds)

  if (length(plist) > 1) {
    plist <- promises::promise_all(.list = plist)
  } else {
    plist <- plist[[1]]
  }

  plist |>
    promises::then(function() {
      invisible()
    }) |>
    promises::finally(
      \(p) {
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
    )

  return(invisible())
}


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
mirai_async_result_handler_win <- function(
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
  while (any(!resolved)) {
    for (i in unresolved_idx) {
      if (inherits(jobs[[i]]$data, "unresolvedValue")) {
        next
      }

      if (resolved[[i]]) {
        next
      }

      j <- mirai::collect_mirai(jobs[[i]])

      if (inherits(j, "miraiError")) {
        cli::cli_abort(
          "{msg}:  {j}",
          class = "mirai_async_error"
        )
      }
      browser()

      rlang::eval_bare(expr)
      # If the job is resolved, mark it as such
      resolved[[i]] <- TRUE
      unresolved_idx <- which(!resolved)
    }
  }
  return(invisible())
}


mirai_async_result_handler <- function(
  jobs,
  ds,
  ...,
  expr,
  msg = "mirai async error"
) {
  if (.Platform$OS.type == "windows") {
    mirai_async_result_handler_win(
      jobs = jobs,
      ds = ds,
      ...,
      expr = expr,
      msg = msg
    )
  } else {
    mirai_async_result_handler_unix(
      jobs = jobs,
      ds = ds,
      ...,
      expr = expr,
      msg = msg
    )
  }
}
