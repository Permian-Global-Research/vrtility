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
    # Existing good settings
    GDAL_VRT_ENABLE_PYTHON = "YES",
    VSI_CACHE = "TRUE",
    GDAL_CACHEMAX = "50%",

    # Optimize VSI caching for COGs
    VSI_CACHE_SIZE = "25000000", # Increase cache size
    VSI_CACHED_CHUNK_SIZE = "262144", # Larger chunk size (256KB)
    CPL_VSIL_CURL_CACHE_SIZE = "25000000", # Explicit curl cache

    # Optimize HTTP connections
    GDAL_HTTP_MAX_RETRY = "5",
    GDAL_HTTP_RETRY_DELAY = "3",
    GDAL_HTTP_BLOCK_SIZE = "16777216", # Increased to 16MB
    GDAL_HTTP_MULTIPLEX = "YES",
    GDAL_HTTP_CONNECTION_POOL = "YES", # NEW: Enable connection pooling
    CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif .TIF", # NEW: Allow both case variants
    CPL_VSIL_CURL_USE_HEAD = "NO", # NEW: Skip HEAD requests
    CPL_VSIL_CURL_NON_CACHED = "YES", # NEW: Better range handling

    # Existing settings
    GDAL_DISABLE_READDIR_ON_OPEN = "YES",
    GDAL_MAX_DATASET_POOL_SIZE = "500",
    GDAL_INGESTED_BYTES_AT_OPEN = "32000",
    GDAL_HTTP_VERSION = "2",
    GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
    GDAL_NUM_THREADS = "ALL_CPUS",

    # Streaming optimization
    VSI_STREAMABLE_CHUNKS_CACHE_SIZE = "25000000" # NEW: Cache for streaming
  )

  gdal_warp_opts <- c(
    # Resampling
    "-r",
    "cubic", # CHANGED: Often better quality/performance balance than 'near'

    # Compression options
    "-co",
    "COMPRESS=LZW", # CHANGED: Often faster than DEFLATE for many datasets
    "-co",
    "PREDICTOR=2",
    "-co",
    "NUM_THREADS=ALL_CPUS",
    "-co",
    "BIGTIFF=YES",

    # Tiling options - optimized for COGs
    "-co",
    "TILED=YES",
    "-co",
    "BLOCKXSIZE=256", # CHANGED: 256 often better aligns with internal COG structure
    "-co",
    "BLOCKYSIZE=256", # CHANGED: 256 often better aligns with internal COG structure
    "-co",
    "COPY_SRC_OVERVIEWS=YES", # NEW: Copy source overviews when available

    # Memory and threading options
    "-wm",
    "50%",
    "-multi",

    # Overview options
    "-ovr",
    "AUTO" # NEW: Use overviews when available
  )

  op <- options()
  op_gdal <- list(
    vrt.gdal.config.options = gdal_config_opts,
    vrt.gdal.warp.options = gdal_warp_opts
  )

  toset <- !(names(op_gdal) %in% names(op))
  if (any(toset)) options(op_gdal[toset])
}
