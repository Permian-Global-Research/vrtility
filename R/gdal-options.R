#' gdal .onLoad options
#' @noRd
#' @keywords internal
#' @details Creates two sets of options:
#' `vrt.gdal.config.options` and `vrt.gdal.warp.options`
gdal_init_opts <- function() {
  gdal_config_opts <- c(
    # Enable Python this is required for vrtility pixel functions
    GDAL_VRT_ENABLE_PYTHON = "YES",
    # cache related
    VSI_CACHE = "TRUE",
    GDAL_CACHEMAX = "50%",
    VSI_CACHE_SIZE = "25000000", # Increase cache size
    # CPL_VSIL_CURL_CHUNK_SIZE = "262144",
    # CPL_VSIL_CURL_CACHE_SIZE = "33554432",

    # Optimize HTTP connections
    GDAL_HTTP_MAX_RETRY = "5",
    GDAL_HTTP_RETRY_DELAY = "3",
    GDAL_HTTP_MULTIPLEX = "YES",
    CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif .TIF",
    CPL_VSIL_CURL_USE_HEAD = "NO",

    # General options
    GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
    GDAL_MAX_DATASET_POOL_SIZE = "500",
    GDAL_INGESTED_BYTES_AT_OPEN = "32000",
    GDAL_HTTP_VERSION = "2",
    GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
    GDAL_NUM_THREADS = "ALL_CPUS"
  )

  gdal_warp_opts <- c(
    # Resampling
    "-r",
    "near",

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
    "BLOCKXSIZE=256",
    "-co",
    "BLOCKYSIZE=256",
    "-co",
    "COPY_SRC_OVERVIEWS=YES",

    # Memory and threading options
    "-wm",
    "50%",
    "-multi",
    # "-wo",
    # "NUM_THREADS=ALL_CPUS",

    # Overview options
    "-ovr",
    "AUTO"
  )

  op <- options()
  op_gdal <- list(
    vrt.gdal.config.options = gdal_config_opts,
    vrt.gdal.warp.options = gdal_warp_opts
  )

  toset <- !(names(op_gdal) %in% names(op))
  if (any(toset)) options(op_gdal[toset])
}
