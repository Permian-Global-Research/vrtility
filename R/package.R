#' GDAL VRT utilities for R
#'
#' THE GDAL VRT (Virtual Format) is an extremely powerful tool for combining
#' and manipulating raster data. This package provides a set of utilities
#' for working with VRTs in R. It is primarily focussed on harnessing the
#' power of VRT pixel functions to carry out complex raster operations. This
#' is achieved through the use of python and in particular the numba library
#' which allows for the creation of fast, compiled functions that can be
#' used in the VRT. All very much a work in progress...
#'
#' @docType package
#' @aliases vrtility-package
#' @name vrtility
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  python_init_checks()
  cache_init_checks()
}

python_init_checks <- function() {
  vrtility_python <- Sys.getenv("VRTILITY_PYTHON", unset = NA)

  if (!is.na(vrtility_python)) {
    reticulate::use_virtualenv(vrtility_python, required = TRUE)
    set_py_env_vals(vrtility_python)
  } else {
    if (reticulate::virtualenv_exists("vrtilitypy")) {
      reticulate::use_virtualenv("vrtilitypy", required = TRUE)
      set_py_env_vals("vrtilitypy")
    } else {
      cli::cli_inform(
        c(
          "!" = "Cannot locate the {cli::style_bold('VRTILITY_PYTHON')} environment",
          "i" = "Run {cli::code_highlight('`build_vrtility_python()`')} to install it"
        ),
        class = "packageStartupMessage"
      )
    }
  }
}

cache_init_checks <- function() {
  op <- options()
  op_vrtility <- list(
    vrt.cache = tempdir()
  )

  toset <- !(names(op_vrtility) %in% names(op))
  if (any(toset)) options(op_vrtility[toset])
}
