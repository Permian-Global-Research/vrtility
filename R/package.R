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
  gdal_init_opts()
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

gdal_init_opts <- function() {
  gdal_config_opts <- c(
    GDAL_VRT_ENABLE_PYTHON = "YES",
    VSI_CACHE = "TRUE",
    GDAL_CACHEMAX = "30%",
    VSI_CACHE_SIZE = "10000000",
    GDAL_HTTP_MULTIPLEX = "YES",
    GDAL_INGESTED_BYTES_AT_OPEN = "32000",
    GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
    GDAL_HTTP_VERSION = "2",
    GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
    GDAL_NUM_THREADS = "ALL_CPUS"
    # , CPL_DEBUG = "ON"
  )

  gdal_warp_opts <- c(
    "-r",
    "near", # experiment with this - may need to be bilinear but we set this in the initial warpedVRTs
    "-co",
    "COMPRESS=DEFLATE",
    "-co",
    "PREDICTOR=2",
    "-co",
    "NUM_THREADS=ALL_CPUS"
  )

  op <- options()
  op_gdal <- list(
    vrt.gdal.config.options = gdal_config_opts,
    vrt.gdal.warp.options = gdal_warp_opts
  )

  toset <- !(names(op_gdal) %in% names(op))
  if (any(toset)) options(op_gdal[toset])
}
