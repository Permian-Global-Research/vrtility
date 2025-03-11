#' A wrapper to call code with the VRTILITY_PYTHON environment
#' @noRd
#' @keywords internal
compute_with_py_env <- function(
  code
) {
  # First, ensure we have the correct paths
  py_bin <- Sys.getenv("VRTILITY_PY_EXECUTABLE", unset = NA)

  if (is.na(py_bin)) {
    cli::cli_abort(c(
      "Cannot locate an appropriate python environment",
      "i" = "You may need to run
        {cli::code_highlight('`vrtility_py_require()`')} to install it"
    ))
  }

  py_env <- dirname(dirname(py_bin))

  # Modified environment setup
  withr::with_envvar(
    new = c(
      # Only set essential variables
      "RETICULATE_PYTHON" = py_bin,
      "VIRTUALENV" = py_env,
      "PATH" = paste(fs::path(py_env, "bin"), Sys.getenv("PATH"), sep = ":")
    ),
    code = code
  )
}
