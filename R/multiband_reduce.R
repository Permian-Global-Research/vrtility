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
#' par(mar = c(0, 0, 1, 0))
#' purrr::iwalk(
#'     list(
#'       geomedian = geomedian(),
#'       medoid = medoid(),
#'       geomedoid = geomedoid(distance_type = "manhattan"),
#'       quantoid = quantoid(probability = 0.2)
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
  reduce_fun,
  outfile,
  config_options,
  creation_options,
  quiet,
  nsplits,
  recollect
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
    quiet
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
  )

  # Initialize output raster
  # TODO: this raster creation is duplicated here and in singleband-many-to-many
  # and call-gdalraster-mirai -> refactor at some point.
  nr <- suppressMessages(gdalraster::rasterFromRaster(
    rt$vrt_template,
    normalizePath(outfile, mustWork = FALSE),
    fmt = "GTiff",
    init = rt$nodataval,
    options = creation_options,
    dtName = rt$data_type
  ))

  ds <- methods::new(gdalraster::GDALRaster, nr, read_only = FALSE)
  on.exit(ds$close(), add = TRUE)
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

  if (mirai::daemons_set()) {
    async_gdalreader_multiband_reduce_read_write(
      blocks_df,
      x,
      ds,
      rt,
      reduce_fun
    )
  } else {
    sequential_gdalreader_multiband_reduce_read_write(
      blocks_df,
      x,
      ds,
      rt,
      reduce_fun,
      quiet
    )
  }

  if (!recollect) {
    return(outfile)
  }

  vrt_collect(
    outfile,
    config_opts = config_options,
    band_descriptions = x$assets
  )
}

#' @title block-level geomedian calculation
#' @description internal functions used by `multiband_reduce` but exported to
#' enable mirai daemon access.
#' @param x A list of lists of matrices, where each inner list represents a
#' time point - the product of `read_block_arrays()`.
#' @return  A list of lists of vectors, where each inner list represents a
#' time point.
#' @keywords internal
#' @noRd
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
#' @noRd
#' @keywords internal
read_block_arrays <- function(x, nbands, xoff, yoff, xsize, ysize) {
  rba_insist <- purrr::insistently(
    function(.x) {
      tvrt <- fs::file_temp(
        tmp_dir = getOption("vrt.cache"),
        ext = "vrt"
      )
      vrt_xml <- xml2::read_xml(.x$vrt)
      xml2::write_xml(vrt_xml, tvrt)
      ds <- methods::new(gdalraster::GDALRaster, tvrt, read_only = TRUE)
      on.exit(ds$close(), add = TRUE)
      # Read bands

      compute_with_py_env(
        gdalraster::read_ds(
          ds,
          bands = seq_len(nbands),
          xoff = xoff,
          yoff = yoff,
          xsize = xsize,
          ysize = ysize,
          out_xsize = xsize,
          out_ysize = ysize,
          as_list = TRUE
        )
      )
    },
    rate = purrr::rate_backoff(
      pause_base = getOption("vrt.pause.base"),
      pause_cap = getOption("vrt.pause.cap"),
      max_times = getOption("vrt.max.times")
    )
  )

  purrr::map(
    x[[1]],
    function(.x) {
      band_data <- rba_insist(.x)
      purrr::map(seq_len(nbands), function(b) {
        matrix(band_data[[b]], nrow = ysize, ncol = xsize)
      })
    }
  )
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
#' @noRd
restructure_cells <- function(cell_vals) {
  restructure_cells_cpp(
    cell_vals
  )
}
