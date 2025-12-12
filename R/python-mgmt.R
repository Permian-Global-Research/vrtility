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
#' not installed by default (currently only numpy), then you can use
#' this function to install the package, making sure to set action as "add".
vrtility_py_require <- function(
  packages = NULL,
  python_version = ">=3.9",
  ...,
  exclude_newer = NULL,
  action = c("add", "remove", "set")
) {
  add_py_lib_to_options(packages)
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


#' Call code that is executed in an environment with access to the vrtility
#' python environment
#' @param code The code to execute
#' @param config_options A named character vector of configuration options to
#' set in the environment before executing the code. eg. generated from
#' \code{\link{gdal_config_options}}
#' @export
#' @rdname vrtility_python
#' @examples
#' compute_with_py_env(print("Hello World"))
compute_with_py_env <- function(
  code,
  config_options = NULL
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

  # ensure we dont pass out format
  config_options <- format_options_for_create(config_options)$opts

  # Parse config_options if they're in "KEY=VALUE" format (unnamed vector)
  if (!is.null(config_options) && is.null(names(config_options))) {
    # Split "KEY=VALUE" strings into named vector
    config_list <- strsplit(config_options, "=", fixed = TRUE)
    config_options <- purrr::map_chr(config_list, ~ .x[2]) |>
      purrr::set_names(purrr::map_chr(config_list, ~ .x[1]))
  }

  # Modified environment setup
  withr::with_envvar(
    new = c(
      config_options,
      GDAL_VRT_ENABLE_PYTHON = "YES",
      RETICULATE_PYTHON = py_bin,
      PATH = paste(fs::path(py_env, "bin"), Sys.getenv("PATH"), sep = ":")
    ),
    code = code
  )
}

#' @title Add additional python libraries to vrtility options
#' @description add_py_lib_to_options adds additional python libraries to the
#' vrtility options so that they can be loaded in mirai daemons.
#' @param lib A character vector of python library names to add.
#' @return Invisible
#' @noRd
#' @keywords internal
add_py_lib_to_options <- function(lib) {
  add_py_libs <- getOption("vrt.add.pylibs", character())
  new_libs <- setdiff(lib, add_py_libs)
  if (length(new_libs) > 0) {
    options(vrt.add.pylibs = c(add_py_libs, new_libs))
  }
  invisible()
}
