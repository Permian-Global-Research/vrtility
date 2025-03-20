#' Create GDAL configuration options.
#' @param GDAL_VRT_ENABLE_PYTHON Enable Python this is required for vrtility pixel functions
#' @param VSI_CACHE cache related
#' @param GDAL_CACHEMAX cache related
#' @param VSI_CACHE_SIZE cache related
#' @param GDAL_HTTP_MAX_RETRY Optimize HTTP connections
#' @param GDAL_HTTP_RETRY_DELAY Optimize HTTP connections
#' @param GDAL_HTTP_MULTIPLEX Optimize HTTP connections
#' @param CPL_VSIL_CURL_ALLOWED_EXTENSIONS Optimize HTTP connections
#' @param CPL_VSIL_CURL_USE_HEAD Optimize HTTP connections
#' @param GDAL_DISABLE_READDIR_ON_OPEN General options
#' @param GDAL_MAX_DATASET_POOL_SIZE General options
#' @param GDAL_INGESTED_BYTES_AT_OPEN General options
#' @param GDAL_HTTP_VERSION General options
#' @param GDAL_HTTP_MERGE_CONSECUTIVE_RANGES General options
#' @param GDAL_NUM_THREADS General options
#' @param ... Additional options to set
#' @rdname gdal_options
#' @export
#' @seealso
#' \href{https://gdal.org/en/stable/user/configoptions.html}{GDAL Configuration Options}
#' @examples
#' gdal_config_opts(GDAL_HTTP_USERPWD = "user:password")
gdal_config_opts <- function(
  GDAL_VRT_ENABLE_PYTHON = "YES",
  VSI_CACHE = "TRUE",
  GDAL_CACHEMAX = "50%",
  VSI_CACHE_SIZE = "268435456",
  GDAL_NUM_THREADS = "ALL_CPUS",
  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
  GDAL_MAX_DATASET_POOL_SIZE = "500",
  GDAL_INGESTED_BYTES_AT_OPEN = "32000",
  CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif .TIF",
  CPL_VSIL_CURL_USE_HEAD = "NO",
  GDAL_HTTP_MAX_RETRY = "5",
  GDAL_HTTP_RETRY_DELAY = "3",
  GDAL_HTTP_MULTIPLEX = "YES",
  GDAL_HTTP_VERSION = "2",
  GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
  ...
) {
  unlist(c(as.list(rlang::current_env()), rlang::dots_list(...)))
}

#' Create GDAL creation options
#' @param output_format Output format equivalent to -of on the CLI. see details
#' @param COMPRESS Compression method
#' @param PREDICTOR Prediction method
#' @param NUM_THREADS Number of threads
#' @param BIGTIFF Use BigTIFF
#' @param TILED Use tiling
#' @param BLOCKXSIZE Block size in X
#' @param BLOCKYSIZE Block size in Y
#' @param COPY_SRC_OVERVIEWS Copy source overviews
#' @param ... Additional -co options to set
#' @rdname gdal_options
#' @export
#' @details
#' output_format, equaivalent to `-of` from the gdaltranslate or gdalwarp CLIs.
#' If NULL, then the output format will be inferred from the file extension.
#' @seealso
#' \href{https://gdal.org/en/stable/drivers/raster/index.html#raster-drivers}{GDAL Raster Drivers}
#' @examples
#' gdal_creation_options(COMPRESS = "JPEG", JPEG_QUALITY = "90")
gdal_creation_options <- function(
  output_format = NULL,
  COMPRESS = "LZW",
  PREDICTOR = "2",
  NUM_THREADS = "ALL_CPUS",
  BIGTIFF = "YES",
  TILED = "YES",
  BLOCKXSIZE = "256",
  BLOCKYSIZE = "256",
  COPY_SRC_OVERVIEWS = "YES",
  ...
) {
  co_args <- c(as.list(rlang::current_env()), rlang::dots_list(...))
  keep_names <- setdiff(names(co_args), "output_format")
  co_args <- co_args[keep_names]
  co_args <- paste0(names(co_args), "=", co_args)
  co_args <- as.vector(rbind("-co", co_args))

  if (!is.null(output_format)) {
    c("-of", output_format, co_args)
  } else {
    co_args
  }
}


#' Create GDAL warp options
#' @param multi Logical indicating whether to use multi-threading, equivalent
#' to -multi on the CLI
#' @param warp_memory Memory to use for warping equivalent to -wm on the CLI
#' @param num_threads Number of threads to use for warping equivalent to -wo
#' NUM_THREADS on the CLI. "ALL_CPUS" (the default) will use all available CPUs,
#' alternartively an integer can be supplied - or NULL to use a single threaded
#' process.
#' @return Character vector of options
#' @rdname gdal_options
#' @export
#' @seealso
#' \href{https://gdal.org/en/stable/programs/gdalwarp.html}{GDAL Warp Options}
#' @examples
#' gdalwarp_options(multi = TRUE, warp_memory = "50%", num_threads = 4)
gdalwarp_options <- function(
  multi = TRUE,
  warp_memory = "50%",
  num_threads = "ALL_CPUS"
) {
  c(
    if (multi) "-multi" else NULL,
    "-wm",
    warp_memory,
    if (!is.null(num_threads)) {
      c("-wo", paste0("NUM_THREADS=", num_threads))
    } else {
      NULL
    }
  )
}

#' Set the GDAL configuration options
#' @param x A named character vector of the configuration options
#' @param scope A character vector of the scope to set the options in. Either
#' "gdalraster" or "sys".
#' @export
#' @rdname gdal_options
#' @examples
#' set_gdal_config(gdal_config_opts())
set_gdal_config <- function(x, scope = c("gdalraster", "sys")) {
  scope <- rlang::arg_match(scope)
  # Store original values

  # Set the config options
  if (scope == "gdalraster") {
    original_values <- purrr::map_chr(
      names(x),
      ~ gdalraster::get_config_option(.x)
    ) |>
      purrr::set_names(names(x))

    purrr::iwalk(x, ~ gdalraster::set_config_option(.y, .x))
  } else {
    original_values <- purrr::map_chr(
      names(x),
      ~ Sys.getenv(.x)
    ) |>
      purrr::set_names(names(x))

    do.call(Sys.setenv, as.list(x))
  }
  purrr::iwalk(x, ~ gdalraster::set_config_option(.y, .x))
  invisible(original_values)
}
