#' @title Image composite reductions that require all bands.
#' @description `multiband_reduce` can be used to create composite reductions
#' that require all band values, such as tyhe geometric median or medoid.
#' composite of a warped VRT collection.
#' @param x A vrt_collection_warped object.
#' @param reduce_fun A function to apply to the data. This function should take
#' a single argument, a matrix where the columns represent the bands of a cell
#' within a raster stack and the rows represent the time series of that cell.
#' The function should return a vector of the same length as the number of
#' bands. See  details.
#' @param outfile The output file path.
#' @param cache_dir The directory to save temporary files. (default should be
#' fine - this is here mainly for explicit privision for mirai daemons)
#' @param config_options A named character vector of GDAL configuration options.
#' @param creation_options A named character vector of GDAL creation options.
#' @param return_internals Logical indicating whether to return the internals of
#' the function (e.g., the jobs object) that is produced before promise
#' evaluation.
#' @param quiet Logical indicating whether to suppress the progress bar.
#' @param nsplits The number of splits to use for the tiling. If NULL, the
#' function will automatically determine the number of splits based on the
#' dimensions of the input data, available memory and the number of active
#' mirai daemons. see details
#' @return A character vector of the output file path.
#' @details
#' We have a lot TODO: info on the reduce_fun options and nsplits etc...
#' @rdname multiband_reduce
#' @examples
#' mirai::daemons(3)
#' s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#'
#' ex_collect <- vrt_collect(s2files)
#'
#' t_block <- ex_collect[[1]][[1]]
#'
#' # export each file with mask.
#' coll_masked <- ex_collect |>
#'   vrt_set_maskfun(
#'     mask_band = "SCL",
#'     mask_values = c(0, 1, 2, 3, 8, 9, 10, 11)
#'   ) |>
#'   vrt_warp(
#'     t_srs = t_block$srs,
#'     te = t_block$bbox,
#'     tr = t_block$res
#'   )
#'
#' # create plots of each of the methods to compare.
#' purrr::walk2(
#'     list(
#'       geomedian(),
#'       medoid(),
#'       geomedoid(distance_type = "manhattan"),
#'       quantoid(probability = 0.2)
#'     ),
#'     list(
#'       "geomedian",
#'       "medoid",
#'       "geomedoid",
#'       "quantoid"
#'     ),
#'     \(.x, .y) {
#'       geomed <- multiband_reduce(
#'         coll_masked,
#'         reduce_fun = .x
#'       )
#'
#'       plot_raster_src(
#'         geomed,
#'         c(3, 2, 1),
#'         axes = FALSE,
#'          main = .y
#'       )
#'     }
#'   )
#'
#' @export
multiband_reduce <- function(
  x,
  reduce_fun = vrtility::medoid(),
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = FALSE,
  nsplits = NULL,
  return_internals = FALSE
) {
  UseMethod("multiband_reduce")
}

#' @noRd
#' @keywords internal
#' @export
multiband_reduce.default <- function(x, ...) {
  cli::cli_abort(
    c(
      "!" = "{class(x)[1]} object types are not supported for `multiband_reduce()`.",
      "i" = "A `vrt_collection_warped` object. is required and can be created 
      with:",
      " " = "{cli::code_highlight('vrt_warp()')}."
    )
  )
}


#' @export
multiband_reduce.vrt_collection_warped <- function(
  x,
  reduce_fun = vrtility::geomedian(),
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  creation_options = gdal_creation_options(),
  quiet = TRUE,
  nsplits = NULL,
  return_internals = FALSE
) {
  # inital assertions and checks.
  multiband_reduce_asserts_init(
    outfile,
    cache_dir,
    config_options,
    creation_options,
    return_internals,
    quiet
  )

  # set gdal config options
  orig_config <- set_gdal_config(c(config_options))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  vrt_template <- vrt_save(x[[1]][[1]])
  tds <- new(gdalraster::GDALRaster, vrt_template)
  xs <- tds$getRasterXSize()
  ys <- tds$getRasterYSize()
  nodataval <- tds$getNoDataValue(1)
  blksize <- tds$getBlockSize(1)
  nbands <- tds$getRasterCount()
  tds$close()

  if (is.null(nsplits)) {
    nsplits <- suggest_n_chunks(ys, xs, nbands, x$n_items)
  }

  blocks_df <- optimise_tiling(
    xs,
    ys,
    blksize,
    nsplits
  )

  # Initialize output raster
  nr <- suppressMessages(gdalraster::rasterFromRaster(
    vrt_template,
    normalizePath(outfile, mustWork = FALSE),
    fmt = "GTiff",
    init = nodataval,
    options = creation_options
  ))

  ds <- methods::new(gdalraster::GDALRaster, nr, read_only = FALSE)
  on.exit(ds$close(), add = TRUE)
  purrr::iwalk(x$assets, function(asset, band) {
    ds$setDescription(
      band = band,
      description = asset
    )
  })

  if (using_daemons()) {
    mirai::everywhere({
      library(vrtility)
    })
  }

  jobs <- purrr::pmap(
    blocks_df,
    carrier::crate(
      function(...) {
        # browser()
        # Extract block parameters correctly from the 1-row dataframe
        block_params <- rlang::dots_list(...)
        ras_band_dat <- read_block_arrays(
          vrt_block,
          nbands = nbands,
          xoff = block_params[["nXOff"]],
          yoff = block_params[["nYOff"]],
          xsize = block_params[["nXSize"]],
          ysize = block_params[["nYSize"]],
          save_dir = cache_dir
        )
        cell_vals <- mdim_reduction_apply(ras_band_dat, reduce_fun)
        list(
          data = restructure_cells(cell_vals),
          block = block_params
        )
      },
      vrt_block = x,
      nbands = nbands,
      cache_dir = cache_dir,
      read_block_arrays = read_block_arrays,
      reduce_fun = reduce_fun,
      mdim_reduction_apply = mdim_reduction_apply,
      restructure_cells = restructure_cells
    ),
    .parallel = using_daemons(),
    .progress = !quiet
  )

  purrr::walk(jobs, function(j) {
    n_cells <- j$block$nXSize * j$block$nYSize
    # Calculate start and end indices for each band
    band_starts <- seq(1, n_cells * nbands, by = n_cells)
    band_ends <- band_starts + n_cells - 1

    j$data[is.na(j$data)] <- nodataval

    purrr::walk(
      seq_len(nbands),
      function(b) {
        start_idx <- band_starts[b]
        end_idx <- band_ends[b]
        ds$write(
          band = b,
          xoff = j$block$nXOff,
          yoff = j$block$nYOff,
          xsize = j$block$nXSize,
          ysize = j$block$nYSize,
          rasterData = j$data[start_idx:end_idx]
        )
      }
    )
  })

  if (return_internals) {
    return(
      list(
        outfile = outfile,
        internals = jobs
      )
    )
  }

  return(outfile)
}

#' @title block-level geomedian calculation
#' @description internal functions used by `multiband_reduce` but exported to
#' enable mirai daemon access.
#' @param x A list of lists of matrices, where each inner list represents a
#' time point - the product of `read_block_arrays()`.
#' @return  A list of lists of vectors, where each inner list represents a
#' time point.
#' @export
#' @rdname geomedian_internal
#' @keywords internal
mdim_reduction_apply <- function(x, mdim_fun) {
  # Get dimensions from first list element
  mast_dim <- dim(x[[1]][[1]])
  n_cells <- prod(mast_dim)
  n_rows <- mast_dim[1]
  n_cols <- mast_dim[2]

  # Create indices in row-major order (to match GDAL)
  cell_indices <- expand.grid(
    row = seq_len(n_rows),
    col = seq_len(n_cols)
  )

  # Extract all band matrices using C++
  band_matrices <- extract_band_matrices(
    x = x,
    row_indices = cell_indices$row,
    col_indices = cell_indices$col,
    n_cells = n_cells,
    n_timepoints = length(x),
    n_bands = length(x[[1]])
  )

  # Process matrices using lapply
  result <- purrr::map(band_matrices, function(bandmatrix) {
    if (nrow(bandmatrix) > 1) {
      mdim_fun(bandmatrix)
    } else if (nrow(bandmatrix) == 1) {
      as.vector(bandmatrix[1, ])
    } else {
      rep(NA_real_, ncol(bandmatrix))
    }
  })

  return(result)
}


#' @title Read VRT block tiles
#' @param x a VRT block object
#' @param nbands number of bands
#' @param xoff x offset
#' @param yoff y offset
#' @param xsize x size
#' @param ysize y size
#' @param save_dir directory to save temporary files
#' @return a list of matrices, where each matrix corresponds to a band
#' @export
#' @rdname geomedian_internal
#' @keywords internal
read_block_arrays <- function(x, nbands, xoff, yoff, xsize, ysize, save_dir) {
  purrr::map(
    x[[1]],
    function(.x) {
      tvrt <- fs::file_temp(tmp_dir = save_dir, ext = "vrt")
      vrt_xml <- xml2::read_xml(.x$vrt)
      xml2::write_xml(vrt_xml, tvrt)
      ds <- methods::new(gdalraster::GDALRaster, tvrt)
      on.exit(ds$close())
      # Read and combine bands
      band_data <- compute_with_py_env(
        purrr::map(
          seq_len(nbands),
          function(b) {
            ds$read(
              band = b,
              xoff = xoff,
              yoff = yoff,
              xsize = xsize,
              ysize = ysize,
              out_xsize = xsize,
              out_ysize = ysize
            )
          }
        )
      )

      purrr::map(seq_len(nbands), function(b) {
        matrix(band_data[[b]], nrow = ysize, ncol = xsize)
      })
    }
  )
}


multiband_reduce_asserts_init <- function(
  outfile,
  cache_dir,
  config_options,
  creation_options,
  return_internals,
  quiet
) {
  v_assert_type(outfile, "outfile", "character", nullok = FALSE)
  v_assert_type(cache_dir, "cache_dir", "character", nullok = FALSE)
  v_assert_type(config_options, "config_options", "character", nullok = FALSE)
  v_assert_type(
    creation_options,
    "creation_options",
    "character",
    nullok = FALSE
  )
  v_assert_type(return_internals, "return_internals", "logical", nullok = FALSE)
  v_assert_type(quiet, "quiet", "logical", nullok = FALSE)

  if (!using_daemons()) {
    cli::cli_inform(
      c(
        "!" = "No miriai daemons detected.",
        " " = "use {cli::code_highlight('mirai::daemons()')} to start daemons",
        "i" = "`multiband_reduce` is a compute-intesntive function and is
        designed to be run in parallel"
      )
    )
  }
}

#' @title Extract band matrices using C++
#' @description Internal C++ function wrapped for R
#' @param x A list of lists of matrices, where each inner list represents a
#' time point - the product of `read_block_arrays()`.
#' @param row_indices A vector of row indices.
#' @param col_indices A vector of column indices.
#' @param n_cells The number of cells.
#' @param n_timepoints The number of time points.
#' @param n_bands The number of bands.
#' @return A list of matrices, where each matrix corresponds to a band.
#' @keywords internal
#' @rdname vrtility_internal
#' @export
extract_band_matrices <- function(
  x,
  row_indices,
  col_indices,
  n_cells,
  n_timepoints,
  n_bands
) {
  extract_band_matrices_cpp(
    x,
    row_indices,
    col_indices,
    n_cells,
    n_timepoints,
    n_bands
  )
}


#' @title Restructure cell values from `mdim_reduction_apply()`
#' @description Internal C++ function wrapped for R
#' @param cell_vals A list of lists of vectors, where each inner list represents
#' a time point - the product of `geomedian_reduction()`.
#' @return A numeric vector of length `n_bands * n_cells`, where `n_bands` is
#' the number of bands and `n_cells` is the number of cells.
#' @keywords internal
#' @rdname vrtility_internal
#' @export
restructure_cells <- function(cell_vals) {
  restructure_cells_cpp(
    cell_vals
  )
}
