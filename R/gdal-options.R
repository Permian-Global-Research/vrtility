#' Create and set GDAL configuration options.
#' @description This function sets GDAL configuration options that can be used to
#' control the behavior of GDAL operations. These options can be used to optimize
#' performance, control caching, and manage HTTP requests (among other things)
#' @param VSI_CACHE Should the Virtual File System (VSI) cache be used?
#' @param VSI_CACHE_SIZE Size of the VSI cache in bytes.
#' @param GDAL_HTTP_MAX_RETRY Maximum number of retries for HTTP requests.
#' @param GDAL_HTTP_RETRY_DELAY Delay between retries in seconds.
#' @param GDAL_HTTP_MULTIPLEX Use HTTP multiplexing?
#' @param CPL_VSIL_CURL_ALLOWED_EXTENSIONS Allowed file extensions for HTTP
#' requests.
#' @param CPL_VSIL_CURL_USE_HEAD Use HTTP HEAD requests?
#' @param CPL_VSIL_CURL_CHUNK_SIZE Chunk size for HTTP requests.
#' @param CPL_VSIL_CURL_CACHE_SIZE Cache size for HTTP requests.
#' @param GDAL_HTTP_COOKIEFILE Path to the cookie file for HTTP requests.
#' @param GDAL_HTTP_COOKIEJAR  Path to the cookie jar for HTTP requests.
#' @param GDAL_DISABLE_READDIR_ON_OPEN Disable directory listing on open?
#' @param GDAL_MAX_DATASET_POOL_SIZE Maximum size of the dataset pool.
#' @param GDAL_INGESTED_BYTES_AT_OPEN Number of bytes to read at open.
#' @param GDAL_HTTP_VERSION HTTP version to use.
#' @param GDAL_HTTP_MERGE_CONSECUTIVE_RANGES Merge consecutive ranges in HTTP
#' requests?
#' @param GDAL_NUM_THREADS Number of threads to use for processing.
#' Default is the number of available cores divided by the number of daemons.
#' @param ... Additional options to set
#' @rdname gdal_options
#' @export
#' @seealso
#' \href{https://gdal.org/en/stable/user/configoptions.html}{GDAL Configuration Options}
#' @details
#' Where a named argument is set to `NULL`, the default GDAL value will be used.
#' These arguments are currently included as NULL because they could in theory
#' improve performance but, from our limited testing they either have no or
#' a negative impact on performance.
#' @examples
#' gdal_config_options(GDAL_HTTP_USERPWD = "user:password")
gdal_config_options <- function(
  VSI_CACHE = "TRUE",
  VSI_CACHE_SIZE = "268435456",
  GDAL_NUM_THREADS = 1,
  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
  CPL_VSIL_CURL_CACHE_SIZE = "1342177280",
  GDAL_HTTP_MAX_RETRY = "10",
  GDAL_HTTP_RETRY_DELAY = "0.5",
  GDAL_HTTP_MULTIPLEX = "YES",
  GDAL_HTTP_VERSION = "2",
  GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
  GDAL_HTTP_COOKIEFILE = "~/.cookies.txt",
  GDAL_HTTP_COOKIEJAR = GDAL_HTTP_COOKIEFILE,
  GDAL_MAX_DATASET_POOL_SIZE = NULL,
  GDAL_INGESTED_BYTES_AT_OPEN = NULL,
  CPL_VSIL_CURL_ALLOWED_EXTENSIONS = NULL,
  CPL_VSIL_CURL_USE_HEAD = "YES",
  CPL_VSIL_CURL_CHUNK_SIZE = NULL,
  ...
) {
  unlist(c(as.list(rlang::current_env()), rlang::dots_list(...)))
}


#' @param output_format Output format equivalent to -of on the CLI. see details
#' @param COMPRESS Compression method
#' @param PREDICTOR Prediction method
#' @param NUM_THREADS Number of threads
#' @param BIGTIFF Use BigTIFF
#' @param TILED Use tiling
#' @param BLOCKXSIZE Block size in X
#' @param BLOCKYSIZE Block size in Y
#' @param COPY_SRC_OVERVIEWS Copy source overviews
#' @param cli_format Logical indicating whether to return the options in a format
#' suitable for passing to system calls (i.e. with -co prefixes)
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
  COMPRESS = "DEFLATE",
  PREDICTOR = "2",
  NUM_THREADS = "ALL_CPUS",
  BIGTIFF = "IF_NEEDED",
  TILED = if (identical(output_format, "COG")) NULL else "YES",
  BLOCKXSIZE = NULL,
  BLOCKYSIZE = NULL,
  COPY_SRC_OVERVIEWS = if (identical(output_format, "COG")) NULL else "YES",
  cli_format = FALSE,
  ...
) {
  #
  if (!is.null(output_format)) {
    check_output_format(output_format)
  }

  co_args <- c(as.list(rlang::current_env()), rlang::dots_list(...))
  co_args <- co_args[!purrr::map_lgl(co_args, is.null)]

  # maybe just a windows thing..
  if (!is.null(co_args$NUM_THREADS)) {
    if (is.na(co_args$NUM_THREADS)) {
      co_args$NUM_THREADS <- "1"
    }
  }

  keep_names <- setdiff(names(co_args), c("output_format", "cli_format"))
  co_args <- co_args[keep_names]

  no_co <- rlang::is_empty(co_args)

  if (no_co) {
    co_args <- character(0)
  }

  if (!no_co) {
    co_args <- paste0(names(co_args), "=", co_args)
  }

  if (cli_format && !no_co) {
    co_args <- as.character(rbind(
      "-co",
      co_args
    ))
  }

  if (!is.null(output_format)) {
    return(c("-of", output_format, co_args))
  } else {
    return(co_args)
  }
}


#' @param multi Logical indicating whether to use multi-threading, equivalent
#' to -multi on the CLI
#' @param warp_memory Memory to use for warping equivalent to -wm on the CLI
#' @param num_threads Number of threads to use for warping equivalent to -wo
#' NUM_THREADS on the CLI. "ALL_CPUS" (the default) will use all available CPUs,
#' alternartively an integer can be supplied - or NULL to use a single threaded
#' process.
#' @param unified_src_nodata Unified source nodata option equivalent to -wo
#' UNIFIED_SRC_NODATA on the CLI. Can be "NO", "YES" or "PARTIAL". Default is
#' "NO" (as was the deafault for earlier versions of GDAL).
#' @return Character vector of options
#' @rdname gdal_options
#' @export
#' @seealso
#' \href{https://gdal.org/en/stable/programs/gdalwarp.html}{GDAL Warp Options}
#' @examples
#' gdalwarp_options(multi = TRUE, warp_memory = "50%", num_threads = 4)
gdalwarp_options <- function(
  multi = FALSE,
  warp_memory = "50%",
  num_threads = if (identical(.Platform$OS.type, "windows")) NULL else 1,
  unified_src_nodata = c("NO", "YES", "PARTIAL")
) {
  unified_src_nodata <- rlang::arg_match(unified_src_nodata)
  c(
    if (multi) "-multi" else NULL,
    "-wm",
    warp_memory,
    if (!is.null(num_threads)) {
      c("-wo", paste0("NUM_THREADS=", num_threads))
    } else {
      NULL
    },
    "-wo",
    paste0("UNIFIED_SRC_NODATA=", unified_src_nodata)
  )
}


#' @param x A named character vector of the configuration options
#' @param scope A character vector of the scope to set the options in. Either
#' "gdalraster" or "sys".
#' @export
#' @rdname gdal_options
#' @examples
#' set_gdal_config(gdal_config_options())
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

#' check GDAL version and warn if it does not meet the minimum requirements
#' @param maj_v_min Minimum major version required
#' @param min_v_min Minimum minor version required
#' @param patch_v_min Minimum patch version required
#' @return NULL, but will inform the user if the GDAL version is not compliant
#' @noRd
#' @keywords internal
check_gdal_and_warn <- function(maj_v_min = 3, min_v_min = 8, patch_v_min = 0) {
  gdal_warn <- function() {
    cli::cli_inform(
      c(
        "!" = "You are using GDAL version {gdalraster::gdal_version()[4]} which 
        is not compliant ",
        " " = "with the minimum recommended version
        {maj_v_min}.{min_v_min}.{patch_v_min}.",
        " " = "Please update GDAL to a newer version to ensure compatibility
        with vrtility."
      ),
      class = "packageStartupMessage"
    )
    return(invisible(FALSE))
  }
  gdal_inform <- function() {
    cli::cli_inform(
      c("v" = "Using GDAL version {gdalraster::gdal_version()[4]}"),
      class = "packageStartupMessage"
    )
    return(invisible(TRUE))
  }

  if (
    !gdalraster::gdal_version_num() >=
      gdalraster::gdal_compute_version(maj_v_min, min_v_min, patch_v_min)
  ) {
    gdal_warn()
    return(invisible(FALSE))
  } else {
    gdal_inform()
  }

  # if we reach here, installed gdal matches the minimum version.
  return(invisible(TRUE))
}


#' @param mem_fraction Fraction of total RAM to use for GDAL cache, default is
#' 0.1 (10% of total RAM)
#' @rdname gdal_options
#' @return set_gdal_cache_max returns (invisibly), a
#' \code{\link[memuse]{memuse}} object - the value set for GDAL_CACHEMAX
#' @details set_gdal_cache_max is a very thin wrapper around
#' \code{\link[gdalraster]{set_cache_max}} that allows you to conveniently
#' set the GDAL_CACHEMAX option as a fraction of the total RAM on your system.
#' @export
#' @examples
#' gcm <- set_gdal_cache_max(0.05)
#' print(gcm)
set_gdal_cache_max <- function(mem_fraction = 0.1) {
  cache_max_val <- memuse::Sys.meminfo()$totalram * mem_fraction

  gdalraster::set_cache_max(as.numeric(cache_max_val))
  cli::cli_inform(
    c(
      "i" = "GDAL_CACHEMAX set to {cache_max_val};
      to change this use
      {cli::code_highlight('vrtility::set_gdal_cache_max()')}"
    ),
    class = "packageStartupMessage"
  )
  return(invisible(cache_max_val))
}


#' Get the list of available GDAL raster drivers
#' @param shortname Logical indicating whether to return the short names of the
#' drivers (default is FALSE)
#' @return A character vector of GDAL raster driver shortnames if
#' \code{shortname = TRUE}, otherwise a data frame with the full driver
#' information.
#' @export
#' @rdname gdal_options
gdal_raster_drivers <- function(shortname = FALSE) {
  gdf <- gdalraster::gdal_formats()
  rgdf <- gdf[gdf$raster, ]

  if (shortname) {
    return(rgdf[["short_name"]])
  }
  return(rgdf)
}


#' Check if Blosc compression is available in the GDAL ZARR driver
#' @return TRUE if Blosc is available, FALSE otherwise
#' @noRd
#' @keywords internal
check_blosc <- function() {
  zarr_compressors <- gdalraster::gdal_get_driver_md("ZARR", "COMPRESSORS")
  if (is.null(zarr_compressors)) {
    return(FALSE)
  }
  return("blosc" %in% strsplit(zarr_compressors, ",")[[1]])
}

#' @param gdal_version Minimum GDAL version to check alongside muparser support
#' @return TRUE if muparser is available, FALSE otherwise
#' @export
#' @rdname gdal_options
#' @details
#' check_muparser can be used to check if the installed gdal version was built
#' with muparser support; muparser is required for derived bands using
#' \code{\link{vrt_derived_block}} or if `options("vrtility.use_muparser")` is
#' set to TRUE when using \code{\link{vrt_set_maskfun}} with muparser-based
#' masks.
check_muparser <- function(gdal_version = "3.11.4") {
  if (!gdal_version_check(gdal_version)) {
    return(FALSE)
  }
  vrt_expr_dialects <- gdalraster::gdal_get_driver_md(
    "VRT",
    "ExpressionDialects"
  )
  if (is.null(vrt_expr_dialects)) {
    return(FALSE)
  }
  return("muparser" %in% strsplit(vrt_expr_dialects, ",")[[1]])
}

#' Check GDAL version against a specified version
#' @param against A character string specifying the version to check against in
#' the format "MAJOR.MINOR.PATCH", e.g. "3.12.0"
#' @return TRUE if the installed GDAL version is greater than or equal to
#' the specified version, FALSE otherwise
#' @noRd
#' @keywords internal
gdal_version_check <- function(against = "3.12.0") {
  v_assert_type(against, "against", "character", nullok = FALSE)
  against_num <- as.numeric(strsplit(against, "\\.")[[1]])
  if (length(against_num) != 3 || anyNA(against_num)) {
    cli::cli_abort(
      c(
        "!" = "`against` must be in the format
        'MAJOR.MINOR.PATCH', e.g. '3.12.0'."
      )
    )
  }

  gdalraster::gdal_version_num() >=
    gdalraster::gdal_compute_version(
      maj = against_num[1],
      min = against_num[2],
      rev = against_num[3]
    )
}


#' Get the output format from GDAL creation options
#' @param creation_options A character vector of GDAL creation options created
#' with \code{\link{gdal_creation_options}}
#' @return Character string of the output format, or NULL if not specified
#' @noRd
#' @keywords internal
format_options_for_create <- function(creation_options) {
  of_index <- which(creation_options == "-of")
  if (length(of_index) == 0) {
    return(list(fmt = NULL, opts = creation_options))
  }
  return(list(
    fmt = creation_options[of_index + 1],
    opts = creation_options[-c(of_index, of_index + 1)]
  ))
}


#' function to check output format
#' @param fmt output format
#' @return NULL, but will error if the format is unsupported
#' @noRd
#' @keywords internal
check_output_format <- function(fmt) {
  if (fmt == "MEM") {
    cli::cli_abort(
      c(
        "Unfortunately the MEM format is not natively supported - because
    of the need to serialize rasters from asynchronous processes.",
        "i" = "If you have any suggestions on how to implement this, please
    open an issue on GitHub."
      ),
      class = "unsupported_gdal_format_error"
    )
  }
  rlang::arg_match(fmt, gdal_raster_drivers(TRUE))
}
