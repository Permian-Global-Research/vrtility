#' @title Image processing along a time series.
#' @description `singleband_m2m` can be used to filter raster time series' or
#' apply smoothing operations. These functions work on a single band at a time.
#' composite of a warped VRT collection.
#' @param x A vrt_collection_warped object.
#' @param m2m_fun A function to apply to the data. This function should take a
#' matrix as input and return a matrix as output. The function should be
#' vectorized over the rows of the matrix, where each row represents a
#' time and columns represent pixels. calcualtions are done at the band level.
#' @param outfile The output file path.
#' @param config_options A named character vector of GDAL configuration options.
#' @param creation_options A named character vector of GDAL creation options.
#' @param quiet Logical indicating whether to suppress the progress bar.
#' @param nsplits The number of splits to use for the tiling. If NULL, the
#' function will automatically determine the number of splits based on the
#' dimensions of the input data, available memory and the number of active
#' mirai daemons. see details
#' @param recollect A logical indicating whether to return the output as a
#' vrt_block or vrt_collection object. default is FALSE and the output is a
#' character string of the output file path.
#' @return A character vector of the output file path.
#' @details
#' We have a lot TODO: here...
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
  outfile = fs::file_temp(ext = "tif"),
  config_options = gdal_config_opts(
    GDAL_HTTP_MAX_RETRY = "3",
    GDAL_HTTP_RETRY_DELAY = "20"
  ),
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
  uniq_pths <- purrr::imap_chr(
    unname(x[[1]]),
    function(.x, .y) {
      if (nchar(.x$date_time) > 0) .x$date_time else as.character(.y)
    }
  ) |>
    unique_fp(outfile)

  ds_list <- purrr::map(
    uniq_pths,
    function(.x) {
      nr <- suppressMessages(gdalraster::rasterFromRaster(
        rt$vrt_template,
        normalizePath(.x, mustWork = FALSE),
        fmt = "GTiff",
        init = rt$nodataval,
        options = creation_options,
        dtName = rt$data_type
      ))
      ds <- methods::new(gdalraster::GDALRaster, nr, read_only = FALSE)
      purrr::iwalk(x$assets, function(asset, band) {
        ds$setDescription(
          band = band,
          description = asset
        )
        if (!is.na(rt$scale_vals[band])) {
          ds$setScale(
            band = band,
            scale = rt$scale_vals[band]
          )
        }
      })
      return(ds)
    }
  )

  if (mirai::daemons_set()) {
    async_gdalreader_singleband_m2m_read_write(
      blocks_df,
      x,
      ds_list,
      m2m_fun = m2m_fun
    )
  } else {
    on.exit(
      # only done in sequential async handles this for promises.
      purrr::walk(ds_list, function(ds) {
        ds$close()
      }),
      add = TRUE
    )

    sequential_gdalreader_singleband_m2m_read_write(
      blocks_df,
      x,
      ds_list,
      m2m_fun = m2m_fun,
      quiet = quiet
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
single_band_reader <- function() {
  purrr::insistently(
    function(blk, block_params) {
      # cli::cli_abort("THIS IS A TEST FAILURE")
      vrtfile <- vrt_block_save_internal(
        blk,
        temp_vrt_dir = getOption("vrt.cache")
      )
      inds <- methods::new(gdalraster::GDALRaster, vrtfile)
      on.exit(inds$close(), add = TRUE)
      compute_with_py_env(
        inds$read(
          band = block_params[["band_n"]],
          xoff = block_params[["nXOff"]],
          yoff = block_params[["nYOff"]],
          xsize = block_params[["nXSize"]],
          ysize = block_params[["nYSize"]],
          out_xsize = block_params[["nXSize"]],
          out_ysize = block_params[["nYSize"]]
        )
      )
    },
    rate = purrr::rate_backoff(
      pause_base = getOption("vrt.pause.base"),
      pause_cap = getOption("vrt.pause.cap"),
      max_times = getOption("vrt.max.times")
    )
  )
}
