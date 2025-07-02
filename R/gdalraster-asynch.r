async_gdalreader_band_read_write <- function(blocks, vrt_file, ds) {
  # Create mirai map with promises
  jobs <- mirai::mirai_map(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      inds <- methods::new(gdalraster::GDALRaster, vrt_file)
      on.exit(inds$close())
      band_data <- blockreader(inds, block_params)
      band_data <- scale_applicator(inds, band_data, block_params)
      return(band_data)
    },
    vrt_file = vrt_file,
    blockreader = blockreader,
    scale_applicator = scale_applicator
  )

  mirai_async_result_handler(
    jobs,
    ds = ds,
    blocks = blocks,
    expr = rlang::expr(
      blockwriter(ds, blocks[i, ], j) # nolint
    ),
    msg = "mirai GDAL read/write error"
  )
}


sequential_gdalreader_band_read_write <- function(blocks, vrt_file, ds, quiet) {
  purrr::pwalk(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      inds <- methods::new(gdalraster::GDALRaster, vrt_file)
      on.exit(inds$close())
      # Read and combine bands
      band_data <- blockreader(inds, block_params)

      j_data <- scale_applicator(inds, band_data, block_params)

      blockwriter(ds, block_params, j_data)
    },
    .progress = !quiet
  )
}

mirai_async_result_handler <- function(
  jobs,
  ds,
  ...,
  expr,
  msg = "mirai async error"
) {
  dots <- rlang::list2(...)
  rlang::env_bind(rlang::current_env(), !!!dots)
  resolved <- logical(length(jobs))
  unresolved_idx <- seq_along(jobs)
  while (any(!resolved)) {
    for (i in unresolved_idx) {
      if (inherits(jobs[[i]]$data, "unresolvedValue")) {
        next
      }

      if (resolved[[i]]) {
        next
      }

      j <- mirai::collect_mirai(jobs[[i]])

      if (inherits(j, "character")) {
        cli::cli_abort(
          "{msg}:  {j}",
          class = "mirai_async_error"
        )
      }

      rlang::eval_bare(expr)
      # If the job is resolved, mark it as such
      resolved[[i]] <- TRUE
      unresolved_idx <- which(!resolved)
    }
  }
}

blockreader <- function(inds, block_params) {
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
}

blockwriter <- function(ds, bps, data) {
  ds$write(
    band = bps[["band_n"]],
    xoff = bps[["nXOff"]],
    yoff = bps[["nYOff"]],
    xsize = bps[["nXSize"]],
    ysize = bps[["nYSize"]],
    rasterData = data
  )
  ds$flushCache() # I think we want this to clear  memory as we go...
}

scale_applicator <- function(inds, band_data, block_params) {
  bscale <- inds$getScale(block_params[["band_n"]])
  if (!is.na(bscale)) {
    band_data <- band_data * bscale
  }
  return(band_data)
}


async_gdalreader_multiband_reduce_read_write <- function(
  blocks,
  vrt_file,
  ds,
  raster_template,
  reduce_fun,
  apply_scale
) {
  jobs <- mirai::mirai_map(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      ras_band_dat <- read_block_arrays(
        vrt_file,
        nbands = nbands,
        xoff = block_params[["nXOff"]],
        yoff = block_params[["nYOff"]],
        xsize = block_params[["nXSize"]],
        ysize = block_params[["nYSize"]]
      )
      cell_vals <- mdim_reduction_apply(ras_band_dat, reduce_fun)
      list(
        data = restructure_cells(cell_vals),
        block = block_params
      )
    },
    vrt_file = vrt_file,
    nbands = raster_template$nbands,
    read_block_arrays = read_block_arrays,
    reduce_fun = reduce_fun,
    mdim_reduction_apply = mdim_reduction_apply,
    restructure_cells = restructure_cells
  )

  mirai_async_result_handler(
    jobs,
    ds = ds,
    blocks = blocks,
    raster_template = raster_template,
    apply_scale = apply_scale,
    expr = rlang::expr(
      multiband_writer(j, ds, raster_template, apply_scale) # nolint
    ),
    msg = "mirai GDAL multi-band reducer error"
  )
}


sequential_gdalreader_multiband_reduce_read_write <- function(
  blocks,
  vrt_file,
  ds,
  raster_template,
  reduce_fun,
  apply_scale,
  quiet
) {
  jobs <- purrr::pmap(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::dots_list(...)
      ras_band_dat <- read_block_arrays(
        vrt_file,
        nbands = raster_template$nbands,
        xoff = block_params[["nXOff"]],
        yoff = block_params[["nYOff"]],
        xsize = block_params[["nXSize"]],
        ysize = block_params[["nYSize"]]
      )
      cell_vals <- mdim_reduction_apply(ras_band_dat, reduce_fun)
      list(
        data = restructure_cells(cell_vals),
        block = block_params
      )
    },
    .progress = !quiet
  )

  purrr::walk(jobs, function(j) {
    multiband_writer(j, ds, raster_template, apply_scale)
  })
}


multiband_writer <- function(j, ds, raster_template, apply_scale) {
  n_cells <- j$block$nXSize * j$block$nYSize
  # Calculate start and end indices for each band
  band_starts <- seq(1, n_cells * raster_template$nbands, by = n_cells)
  band_ends <- band_starts + n_cells - 1

  j$data[is.na(j$data)] <- raster_template$nodataval

  purrr::walk(seq_len(raster_template$nbands), function(b) {
    start_idx <- band_starts[b]
    end_idx <- band_ends[b]

    bscale <- raster_template$scale_vals[b]
    bdata <- if (!is.na(bscale) && apply_scale) {
      j$data[start_idx:end_idx] * bscale
      # Get the data for this band
      band_data <- j$data[start_idx:end_idx]

      # Create mask for valid data (not equal to nodata)
      valid_mask <- band_data != raster_template$nodataval

      # Apply scale only to valid data
      band_data[valid_mask] <- band_data[valid_mask] * bscale

      band_data
    } else {
      j$data[start_idx:end_idx]
    }

    blockwriter(ds, c(band_n = b, j$block), bdata)
  })
}


async_gdalreader_singleband_m2m_read_write <- function(
  blocks,
  vrt_collection,
  ds_list,
  m2m_fun,
  apply_scale,
  scale_values
) {
  jobs <- mirai::mirai_map(
    blocks,
    function(...) {
      block_params <- rlang::dots_list(...)

      band_data <- purrr::map(
        vrt_collection[[1]],
        ~ sb_reader_fun(.x, block_params)
      )

      bdm <- do.call(rbind, band_data)
      hamp_bdm <- m2m_fun(bdm)

      return(list(
        band_data = matrix_to_rowlist(hamp_bdm),
        block_params = block_params
      ))
    },
    vrt_collection = vrt_collection,
    sb_reader_fun = single_band_reader(),
    vrt_block_save_internal = vrt_block_save_internal,
    compute_with_py_env = compute_with_py_env,
    m2m_fun = m2m_fun,
    matrix_to_rowlist = matrix_to_rowlist
  )

  mirai_async_result_handler(
    jobs,
    ds = ds_list,
    blocks = blocks,
    apply_scale = apply_scale,
    scale_values = scale_values,
    expr = rlang::expr(
      singleband_m2m_writer(ds, j, apply_scale, scale_values) # nolint
    ),
    msg = "mirai GDAL single-band many-to-many error"
  )
}

sequential_gdalreader_singleband_m2m_read_write <- function(
  blocks,
  vrt_collection,
  ds_list,
  m2m_fun,
  apply_scale,
  scale_values,
  quiet
) {
  sb_reader_fun <- single_band_reader()

  jobs <- purrr::pmap(
    blocks,
    function(...) {
      block_params <- rlang::dots_list(...)
      band_data <- purrr::map(
        vrt_collection[[1]],
        ~ sb_reader_fun(.x, block_params)
      )

      bdm <- do.call(rbind, band_data)
      hamp_bdm <- m2m_fun(bdm)

      return(list(
        band_data = matrix_to_rowlist(hamp_bdm),
        block_params = block_params
      ))
    },
    .progress = !quiet
  )

  purrr::walk(jobs, function(j) {
    singleband_m2m_writer(ds_list, j, apply_scale, scale_values)
  })
}

singleband_m2m_writer <- function(ds_list, j, apply_scale, scale_values) {
  .params <- j$block_params
  purrr::pwalk(
    list(.ds = ds_list, .data = j$band_data),
    function(.ds, .data) {
      bscale <- scale_values[.params$band_n]
      if (!is.na(bscale) && apply_scale) {
        .data <- .data * bscale
      }
      # Write the band data to the output raster
      blockwriter(.ds, .params, .data)
    }
  )
}
