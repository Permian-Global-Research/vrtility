#' @title block-level geomedian calculation
#' @description internal functions used by `reduce_to_geomedian` but exported to
#' enable mirai daemon access.
#' @param x A list of lists of matrices, where each inner list represents a
#' time point - the product of `read_block_arrays()`.
#' @return  A list of lists of vectors, where each inner list represents a
#' time point.
#' @export
#' @rdname geomedian_internal
#' @keywords internal
geomedian_reduction <- function(x) {
  # Get dimensions from first list element
  mast_dim <- dim(x[[1]][[1]])
  n_cells <- prod(mast_dim)
  n_rows <- mast_dim[1]
  n_cols <- mast_dim[2]

  # Pre-allocate result list
  result <- vector("list", n_cells)
  # Create indices in row-major order (to match GDAL)
  cell_indices <- expand.grid(
    row = seq_len(n_rows),
    col = seq_len(n_cols)
  )

  # Process each cell
  for (k in seq_len(n_cells)) {
    i <- cell_indices$row[k]
    j <- cell_indices$col[k]

    # Extract values from each time point and band

    bandmatrix <- t(vapply(
      x, # List of time points
      function(time_point) {
        vapply(
          time_point, # List of bands
          function(m) m[i, j],
          numeric(1)
        )
      },
      numeric(length(x[[1]]))
    ))

    bandmatrix <- bandmatrix[
      !apply(bandmatrix, 1, function(x) all(is.na(x))),
      ,
      drop = FALSE
    ]

    # Only compute Gmedian if we have data
    if (nrow(bandmatrix) > 1) {
      colmeds <- Rfast::colMedians(bandmatrix, na.rm = TRUE)

      # creating the Gamma constant.
      gamma <- 18 +
        (mean(Rfast::colVars(bandmatrix, std = TRUE, na.rm = TRUE)) /
          mean(colmeds)) *
          10

      result[[k]] <- as.vector(
        Gmedian::Gmedian(
          bandmatrix,
          init = colmeds,
          nstart = 100,
          gamma = gamma,
          alpha = 0.9
        )
      )
      #TODO: maybe we also allow for the Weiszfeld method?
      # not particularly good at cloud filtering...
      # result[[k]] <- as.vector(
      #   Gmedian::Weiszfeld(
      #     bandmatrix,
      #     nitermax = 1000
      #   )$median
      # )
    } else if (nrow(bandmatrix) == 1) {
      result[[k]] <- as.vector(bandmatrix[1, ])
    } else {
      result[[k]] <- rep(NA_real_, length(x[[1]]))
    }
  }

  return(result)
}

#' @title Restructure cell values from `geomedian_reduction()`
#' @param cell_vals A list of lists of vectors, where each inner list represents
#' a time point - the product of `geomedian_reduction()`.
#' @return A numeric vector of length `n_bands * n_cells`, where `n_bands` is
#' the number of bands and `n_cells` is the number of cells.
#' @rdname geomedian_internal
#' @export
#' @keywords internal
restructure_cells <- function(cell_vals) {
  # Get dimensions
  n_bands <- length(cell_vals[[1]])
  n_cells <- length(cell_vals)

  # Pre-allocate result vector
  result <- numeric(length = n_bands * n_cells)

  # Calculate start indices for each band
  band_starts <- seq(1, length(result), by = n_cells)
  band_ends <- band_starts + n_cells - 1
  # Fill result vector by band
  for (band in seq_len(n_bands)) {
    start_idx <- band_starts[band]
    end_idx <- band_ends[band]
    result[start_idx:end_idx] <- vapply(
      cell_vals,
      function(m) {
        tryCatch(
          m[band],
          error = function(e) {
            NA_real_
          }
        )
      },
      numeric(1)
    )
  }

  result
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
      band_data <- vrtility::compute_with_py_env(
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

#' Create a geometric median composite.
#' This function creates geometric median (aka the spatial or l1 median)
#' composite of a warped VRT collection.
#' @param x A vrt_collection_warped object.
#' @param outfile The output file path.
#' @param cache_dir The directory to save temporary files. (default should be
#' fine - this is here mainly for explicit privision for mirai daemons)
#' @param config_options A named character vector of GDAL configuration options.
#' @param compression The compression method to use for the output file.
#' @param return_internals Logical indicating whether to return the internals of
#' the function (e.g., the jobs object) that is produced before promise
#' evaluation.
#' @param quiet Logical indicating whether to suppress the progress bar.
#' @return A character vector of the output file path.
#' @export
reduce_to_geomedian <- function(
  x,
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  compression = "LZW",
  return_internals = FALSE,
  quiet = FALSE
) {
  v_assert_type(outfile, "outfile", "character", nullok = FALSE)
  v_assert_type(cache_dir, "cache_dir", "character", nullok = FALSE)
  v_assert_type(config_options, "config_options", "character", nullok = FALSE)
  v_assert_type(compression, "compression", "character", nullok = FALSE)
  v_assert_type(return_internals, "return_internals", "logical", nullok = FALSE)
  v_assert_type(quiet, "quiet", "logical", nullok = FALSE)

  UseMethod("reduce_to_geomedian")
}

#' @export
reduce_to_geomedian.vrt_collection_warped <- function(
  x,
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  config_options = gdal_config_opts(),
  compression = "LZW",
  return_internals = FALSE,
  quiet = FALSE
) {
  orig_config <- set_gdal_config(c(config_options))
  on.exit(set_gdal_config(orig_config), add = TRUE)

  # get template params
  vrt_template <- vrt_save(x[[1]][[1]])
  ds <- new(gdalraster::GDALRaster, vrt_template)
  xs <- ds$getRasterXSize()
  ys <- ds$getRasterYSize()
  nodataval <- ds$getNoDataValue(1)
  blksize <- ds$getBlockSize(1)
  nbands <- ds$getRasterCount()
  blocks_df <- as.data.frame(get_tiles(xs, ys, blksize[1], blksize[2]))
  blocks <- split(blocks_df, seq_len(nrow(blocks_df)))
  ds$close()
  # Initialize output raster

  suppressMessages(gdalraster::rasterFromRaster(
    vrt_template,
    outfile,
    fmt = "GTiff",
    init = nodataval,
    options = c(
      glue::glue("COMPRESS={compression}"), # Good balance of compression/speed
      "PREDICTOR=2",
      "TILED=YES", # Enable tiling
      glue::glue(
        "BLOCKXSIZE={blksize[1]}"
      ),
      glue::glue(
        "BLOCKYSIZE={blksize[2]}"
      ),
      "BIGTIFF=IF_NEEDED" # Automatic large file handling
    )
  ))

  # inital data open in advance of promise eval.
  ds <- new(gdalraster::GDALRaster, outfile, read_only = FALSE)
  on.exit(ds$close(), add = TRUE)

  # Create mirai map with promises
  jobs <- mirai::mirai_map(
    blocks,
    function(b) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- unlist(b)
      ras_band_dat <- vrtility::read_block_arrays(
        x,
        nbands = nbands,
        xoff = block_params["nXOff"],
        yoff = block_params["nYOff"],
        xsize = block_params["nXSize"],
        ysize = block_params["nYSize"],
        save_dir = cache_dir
      )
      cell_vals <- vrtility::geomedian_reduction(ras_band_dat)
      list(
        data = vrtility::restructure_cells(cell_vals),
        block = c(
          xoff = b["nXOff"],
          yoff = b["nYOff"],
          xsize = b["nXSize"],
          ysize = b["nYSize"]
        )
      )
    },
    x = x,
    nbands = nbands,
    cache_dir = cache_dir,
    .promise = list(
      # On success
      function(result) {
        if (!ds$isOpen()) {
          ds <- new(gdalraster::GDALRaster, outfile, read_only = FALSE)
        }
        n_cells <- result$block[[3]] * result$block[[4]]

        # Calculate start and end indices for each band
        band_starts <- seq(1, n_cells * nbands, by = n_cells)
        band_ends <- band_starts + n_cells - 1

        result$data[is.na(result$data)] <- nodataval

        purrr::walk(
          seq_len(nbands),
          function(b) {
            start_idx <- band_starts[b]
            end_idx <- band_ends[b]
            ds$write(
              band = b,
              xoff = result$block[[1]],
              yoff = result$block[[2]],
              xsize = result$block[[3]],
              ysize = result$block[[4]],
              rasterData = result$data[start_idx:end_idx]
            )
          }
        )
        return(outfile)
      },
      # On error
      function(error) {
        stop(
          c(
            "x" = "Failed to process block...",
            " " = "{conditionMessage(error)}"
          )
        )
      }
    )
  )

  if (!quiet) {
    result <- jobs[.progress]
  } else {
    result <- jobs[]
  }

  if (return_internals) {
    return(
      list(
        outfile = outfile,
        internals = result
      )
    )
  }

  return(outfile)
}
