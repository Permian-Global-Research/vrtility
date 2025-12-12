#' @title Temporal filtering and smoothing for raster time series
#' @description Apply temporal operations to each pixel's time series in a
#' warped VRT collection. This function processes each band independently,
#' applying a user-defined function across the temporal dimension (e.g.,
#' filtering outliers, smoothing, gap-filling).
#'
#' @param x A `vrt_collection_warped` object. All images must be aligned to the
#' same grid (use [vrt_warp()] first).
#' @param m2m_fun A function that operates on time series data. The function
#' receives a matrix where rows represent time steps and columns represent
#' pixels, and must return a matrix of the same dimensions. See \code{hampel_filter}
#' for an example. Calculations are performed independently for each band.
#' @param outfile Output file path(s). Can be a single path (used as template)
#' or a vector of paths (one per time step). Defaults to temporary files.
#' @param config_options A named character vector of GDAL configuration options.
#' See \code{gdal_config_opts()}.
#' @param creation_options A named character vector of GDAL creation options for
#' output files. See \code{gdal_creation_options()}.
#' @param quiet Logical. If `FALSE`, displays a progress bar during processing.
#' @param nsplits Integer specifying the number of spatial tiles to process. If
#' `NULL` (default), automatically determines optimal tiling based on input
#' dimensions, available memory, and number of parallel workers.
#' @param recollect Logical. If `TRUE`, returns a `vrt_collection` object
#' containing the processed images. If `FALSE` (default), returns a character
#' vector of output file paths.
#'
#' @return If `recollect = FALSE`, a character vector of output file paths (one
#' per time step). If `recollect = TRUE`, a `vrt_collection` object.
#'
#' @details
#' ## Processing Model
#'
#' `singleband_m2m()` materializes the virtual raster collection by:
#'
#' 1. **Spatial tiling**: Divides the spatial extent into tiles for efficient
#' memory usage
#' 2. **Band-wise processing**: For each band and tile, extracts the complete
#' time series (all images)
#' 3. **Temporal operations**: Applies `m2m_fun` to the time series matrix
#' (rows = time, cols = pixels)
#' 4. **Output writing**: Writes filtered results to disk as GeoTIFF files
#'
#' ## Common Use Cases
#'
#' - **Outlier removal**: Remove cloud contamination or sensor artifacts using
#' [hampel_filter()]
#' - **Temporal smoothing**: Apply moving average or median filters
#' - **Gap filling**: Interpolate missing values in time series
#' - **Temporal compositing**: Create median/mean composites while preserving
#' all time steps
#'
#' ## Performance Considerations
#'
#' - Processing is parallelized if `mirai` daemons are active (see
#' \code{daemons}[mirai]).
#' - Automatic tiling balances memory usage against I/O overhead
#' - Each output image retains original metadata (timestamps, band descriptions)
#' - Use `recollect = TRUE` to chain with additional VRT operations
#'
#' @seealso [hampel_filter()] for outlier detection, [vrt_warp()] to prepare
#' aligned collections, [vrt_derived_block()] for band math before filtering
#'
#' @examplesIf interactive()
#' #  Set up asynchronous workers to parallelise vrt_collect and vrt_set_maskfun
#' mirai::daemons(4)
#'
#' bbox <- gdalraster::bbox_from_wkt(
#'   wkt = "POINT (144.3 -7.6)",
#'   extend_x = 0.05,
#'   extend_y = 0.05
#' )
#'
#' te <- bbox_to_projected(bbox)
#' trs <- attr(te, "wkt")
#'
#' s2_stac <- hls_stac_query(
#'   bbox = bbox,
#'   start_date = "2023-01-01",
#'   end_date = "2024-12-31",
#'   max_cloud_cover = 40,
#'   assets = c("B02", "B03", "B04", "Fmask")
#' )
#' # number of items:
#' length(s2_stac$features)
#'
#' collection <- vrt_collect(s2_stac) |>
#'   vrt_set_maskfun(
#'     mask_band = "Fmask",
#'     mask_values = c(0, 1, 2, 3),
#'     build_mask_pixfun = build_bitmask()
#'   ) |>
#'   vrt_warp(t_srs = trs, te = te, tr = c(30, 30))
#'
#' # Apply Hampel filter to remove outliers
#' filtered <- collection |>
#'   singleband_m2m(
#'     m2m_fun = hampel_filter(k = 3L, t0 = 0, impute_na = TRUE),
#'     recollect = TRUE
#'   )
#' withr::with_par(list(mfrow = c(2, 1)), {
#'   plot(collection, item = 10, bands = c(3, 2, 1), main = "Original")
#'   plot(filtered, item = 10, bands = c(3, 2, 1), main = "Hampel Filtered")
#' })
#'
#' # Custom temporal function: 5-image moving median
#' moving_mean <- function(x, width = 5) {
#'   n_time <- nrow(x)
#'   n_pixels <- ncol(x)
#'
#'   # Pre-allocate result matrix
#'   result <- matrix(NA_real_, nrow = n_time, ncol = n_pixels)
#'
#'   # Calculate half-window size for centering
#'   half_width <- floor(width / 2)
#'
#'   # For each time step where we can compute a full window
#'   for (t in (half_width + 1):(n_time - half_width)) {
#'     window_idx <- (t - half_width):(t + half_width)
#'     result[t, ] <- colMeans(x[window_idx, , drop = FALSE], na.rm = TRUE)
#'   }
#'
#'   return(result)
#' }
#'
#'   smoothed <- singleband_m2m(
#'     collection,
#'     m2m_fun = moving_mean,
#'     recollect = TRUE
#'   )
#'
#'
#' withr::with_par(list(mfrow = c(1, 2)), {
#'   plot(collection, item = 10, bands = c(3, 2, 1), main = "Original")
#'   plot(smoothed, item = 10, bands = c(3, 2, 1), main = "5-Image Moving Mean")
#' })
#' @rdname singleband-many-to-many
#' @export
singleband_m2m <- function(
  x,
  m2m_fun,
  outfile,
  config_options,
  creation_options,
  quiet,
  nsplits,
  recollect
) {
  UseMethod("singleband_m2m")
}

#' @noRd
#' @keywords internal
#' @export
singleband_m2m.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{class(x)[1]} object types are not supported for `singleband_m2m()`.",
      "i" = "A `vrt_collection_warped` object. is required and can be created 
      with:",
      " " = "{cli::code_highlight('vrt_warp()')}."
    )
  )
}


#' @export
singleband_m2m.vrt_collection_warped <- function(
  x,
  m2m_fun = hampel_filter(k = 1L, t0 = 0, impute_na = FALSE),
  outfile = fs::file_temp(tmp_dir = getOption("vrt.cache"), ext = "tif"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = TRUE,
  nsplits = NULL,
  recollect = FALSE
) {
  daemon_setup(gdal_config = config_options)
  # inital assertions and checks.
  gdalraster_engine_asserts_init(
    outfile,
    config_options,
    creation_options,
    quiet,
    fname = "singleband_m2m"
  )

  # set gdal config options
  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  rt <- raster_template_params(x[[1]][[1]])

  if (is.null(nsplits)) {
    nsplits <- suggest_n_chunks(rt$ys, rt$xs, rt$nbands, x$n_items)
  }

  blocks_df <- optimise_tiling(
    rt$xs,
    rt$ys,
    rt$blksize,
    nsplits
  ) |>
    merge(data.frame(band_n = seq_len(rt$nbands)))

  # Initialize output raster
  dttm_vec <- purrr::imap_chr(
    unname(x[[1]]),
    function(.x, .y) {
      if (nchar(.x$date_time) > 0) .x$date_time else as.character(.y)
    }
  )
  uniq_pths <- unique_fp(dttm_vec, outfile)

  ds_list <- purrr::map2(
    uniq_pths,
    dttm_vec,
    function(.x, .y) {
      nr <- suppressMessages(gdalraster::rasterFromRaster(
        rt$vrt_template,
        normalizePath(.x, mustWork = FALSE),
        fmt = "GTiff",
        init = rt$nodataval,
        options = creation_options,
        dtName = rt$data_type
      ))
      ds <- methods::new(gdalraster::GDALRaster, nr, read_only = FALSE)
      set_desc_scale_offset(x, ds, rt)
      if (nzchar(.y)) {
        set_dttm_metadata(nr, .y, .median = FALSE)
      }

      return(ds)
    }
  )

  on.exit(
    purrr::walk(ds_list, function(ds) {
      ds$close()
    }),
    add = TRUE
  )

  if (mirai::daemons_set()) {
    async_gdalreader_singleband_m2m_read_write(
      blocks_df,
      x,
      ds_list,
      m2m_fun = m2m_fun,
      config_options = config_options
    )
  } else {
    sequential_gdalreader_singleband_m2m_read_write(
      blocks_df,
      x,
      ds_list,
      m2m_fun = m2m_fun,
      quiet = quiet,
      config_options = config_options
    )
  }

  if (!recollect) {
    return(uniq_pths)
  }

  vrt_collect(
    uniq_pths,
    config_opts = config_options,
    band_descriptions = x$assets,
    datetimes = x$date_time
  )
}

#' @title Convert a matrix to a list of vectors
#' @description Convert a matrix to a list of vectors, where each vector
#' represents a row of the matrix.
#' @param x A matrix.
#' @return A list of vectors, where each vector represents a row of the
#' matrix.
#' @export
#' @keywords internal
#' @rdname vrtility_internal
matrix_to_rowlist <- function(x) {
  matrix_to_rowlist_cpp(x)
}


#' @description `hampel_filter` is used tocreate a function to filter band-level
#' outliers in time-series using the Hampel filter. to be provided to
#' `singleband_m2m()`.
#' @param k The number of neighboring points to consider on each side of the
#' current point. window length 2*k+1 in indices.
#' @param t0 threshold, default is 3 (Pearson's rule), see below.
#' @param impute_na Logical indicating whether to impute NA values. If `TRUE`,
#' the function will impute NA values using the nearest prior non-NA value. If
#' `FALSE`, NA values will be returned in their original positions.
#' @return A function to be used with `singleband_m2m()` to remove outliers
#' from a raster time series.
#' @details
#' (details from the `pracma::hampel`) The ‘median absolute deviation’ computation is
#' done in the (-k...k) vicinity of each point at least k steps away from the
#' end points of the interval. At the lower and upper end the time series values
#' are preserved.
#'
#' A high threshold makes the filter more forgiving, a low one will declare
#' more points to be outliers. t0<-3 (the default) corresponds to Ron Pearson's
#' 3 sigma edit rule, t0<-0 to John Tukey's median filter.
#'
#' The implementation of the hampel filter is based on the pracma package but
#' implemented in C++. It also handles NA values differently, First, NA values
#' are removed from the data before applying the filter. If `impute_na` is
#' `TRUE`, the function will impute NA values using the nearest prior non-NA
#' value. If `impute_na` is `FALSE`, NA values will be returned in their
#' original positions.
#' @rdname singleband-many-to-many
#' @export
#'
hampel_filter <- function(k = 1L, t0 = 3, impute_na = FALSE) {
  v_assert_type(k, "k", "integer", nullok = FALSE)
  v_assert_within_range(k, "k", 1L, Inf)
  v_assert_type(t0, "t0", "numeric", nullok = FALSE)
  v_assert_within_range(t0, "t0", 0, Inf)
  v_assert_type(impute_na, "impute_na", "logical", nullok = FALSE)
  v_assert_length(impute_na, "impute_na", 1)
  function(x) hampel_filter_matrix_cpp(x, k, t0, impute_na)
}

#' band reader function factory
#' @description `single_band_reader` is an internal function exported for use
#' within singleband_m2m() when used in parallel.
#' @noRd
#' @keywords internal
single_band_reader <- function(blk, block_params, config_options) {
  vrtfile <- blk$vrt_src
  #TODO: would the following be better for mirai?
  # if (!exists("inds")) {
  #   inds <<- methods::new(gdalraster::GDALRaster, vrt_file) # nolint
  # }
  inds <- methods::new(gdalraster::GDALRaster, vrtfile)
  on.exit(inds$close(), add = TRUE)
  blockreader(inds, block_params, config_options)
}
