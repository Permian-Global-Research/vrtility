#' @title Setup the vrtility Python environment
#' @description A very thin wrapper around the `reticulate::py_install` function
#' to set up the necessary python environment and then set some options required
#' by the vrtility package.
#' environment
#' @inheritParams reticulate::py_require
#' @return Invisible
#' @export
#' @rdname vrtility_python
#' @details In general this function shouldnt be required but if, for example,
#' you wish to use a custom python pixel function which uses a python package
#' not installed by default (currently only numpy and numba), then you can use
#' this function to install the package, making sure to set action as "add".
vrtility_py_require <- function(
  packages = NULL,
  python_version = ">=3.9",
  ...,
  exclude_newer = NULL,
  action = c("add", "remove", "set")
) {
  reticulate::py_require(
    packages = packages,
    python_version = python_version,
    ...,
    exclude_newer = exclude_newer,
    action = action
  )
  set_py_env_vals()
  invisible()
}


#' @title Set Python environment variables
#' @description set_py_env_vals sets the environment variables required by the
#' vrtility package - typically not required.
#' @export
#' @rdname vrtility_python
#' @details
#' set_py_env_vals is only required if you are running reticulate::py_env in
#' some non-standard way. In general it is a lot easier to  use #
#' vrtility_py_require which will automatically set the necessary options.
set_py_env_vals <- function() {
  sys <- reticulate::import("sys")
  Sys.setenv(VRTILITY_PY_EXECUTABLE = sys$executable)
  Sys.setenv(VRTILITY_PY_VERSION_MAJOR = sys$version_info$major)
  Sys.setenv(VRTILITY_PY_VERSION_MINOR = sys$version_info$minor)
  invisible(c(
    VRTILITY_PY_EXECUTABLE = Sys.getenv("VRTILITY_PY_EXECUTABLE"),
    VRTILITY_PY_VERSION_MAJOR = Sys.getenv("VRTILITY_PY_VERSION_MAJOR"),
    VRTILITY_PY_VERSION_MINOR = Sys.getenv("VRTILITY_PY_VERSION_MINOR")
  ))
}


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
