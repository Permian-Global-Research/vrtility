raster_reduce_geomedian <- function(x) {
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
      row_sums <- rowSums(bandmatrix, na.rm = TRUE)
      result[[k]] <- as.vector(
        Gmedian::Gmedian(
          bandmatrix,
          init = robustbase::colMedians(bandmatrix, na.rm = TRUE),
          nstart = 500,
          gamma = round(max(row_sums / ncol(bandmatrix), na.rm = TRUE) * 0.05),
          alpha = 0.9
        )
      )

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

  result
}


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


reduce_to_geomedian <- function(
  x,
  blocks,
  nbands,
  outfile = fs::file_temp(ext = "tif"),
  cache_dir = getOption("vrt.cache"),
  mem_limit = 60
) {
  # Initialize output raster
  vrt_template <- vrt_save(x[[1]][[1]])
  ds <- new(gdalraster::GDALRaster, vrt_template)
  nodataval <- ds$getNoDataValue(1)
  blksize <- ds$getBlockSize(1)
  ds$close()

  suppressMessages(gdalraster::rasterFromRaster(
    vrt_template,
    outfile,
    fmt = "GTiff",
    init = nodataval,
    options = c(
      "COMPRESS=LZW", # Good balance of compression/speed
      "PREDICTOR=2", # Helps with continuous data
      "TILED=YES", # Enable tiling for better read performance
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
      ras_band_dat <- read_block_arrays(
        x,
        nbands = nbands,
        xoff = block_params["nXOff"],
        yoff = block_params["nYOff"],
        xsize = block_params["nXSize"],
        ysize = block_params["nYSize"],
        save_dir = cache_dir
      )
      cell_vals <- raster_reduce_geomedian(ras_band_dat)
      list(
        data = restructure_cells(cell_vals),
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
        warning(sprintf("Failed block: %s", conditionMessage(error)))
      }
    )
  )

  # Return the jobs object for monitoring
  jobs
}
