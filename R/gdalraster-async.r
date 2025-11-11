#' @title Asynchronous GDAL band read/write
#' @description Reads and writes GDAL raster bands in blocks asynchronously.
#' used interally by `vrt_compute()`.
#' @param blocks A dataframe with block parameters.
#' @param vrt_file The VRT file path.
#' @param ds The GDALRaster dataset to write to - must be open and with
#' `readonly=FALSE`.
#' @return invisible
#' @keywords internal
#' @noRd
async_gdalreader_band_read_write <- function(blocks, vrt_file, ds) {
  # Create mirai map with promises
  jobs <- mirai::mirai_map(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::list2(...)
      inds <- methods::new(gdalraster::GDALRaster, vrt_file)
      on.exit(inds$close())
      band_data <- blockreader(inds, block_params)
      return(band_data)
    },
    vrt_file = vrt_file,
    blockreader = blockreader
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
  ds$close()
  return(invisible())
}

#' @title Sequential GDAL band read/write
#' @description Reads and writes GDAL raster bands in blocks sequentially.
#' @param blocks A dataframe with block parameters.
#' @param vrt_file The VRT file path.
#' @param ds The GDALRaster dataset to write to - must be open and with
#' `read_only=FALSE`.
#' @param quiet Logical, if TRUE suppresses progress messages.
#' @return invisible
#' @keywords internal
#' @noRd
sequential_gdalreader_band_read_write <- function(blocks, vrt_file, ds, quiet) {
  purrr::pwalk(
    blocks,
    function(...) {
      # Extract block parameters correctly from the 1-row dataframe
      block_params <- rlang::list2(...)
      inds <- methods::new(gdalraster::GDALRaster, vrt_file)
      on.exit(inds$close())
      # Read and combine bands
      band_data <- blockreader(inds, block_params)

      blockwriter(ds, block_params, band_data)
    },
    .progress = !quiet
  )
  return(invisible())
}

#' @title Block reader function
#' @description Reads a block of data from a GDALRaster dataset.
#' @param inds The GDALRaster dataset object.
#' @param block_params A list of block parameters including band number and offsets.
#' @return A matrix of raster data for the specified block.
#' @keywords internal
#' @noRd
blockreader <- function(inds, block_params) {
  return(compute_with_py_env(
    inds$read(
      band = block_params[["band_n"]],
      xoff = block_params[["nXOff"]],
      yoff = block_params[["nYOff"]],
      xsize = block_params[["nXSize"]],
      ysize = block_params[["nYSize"]],
      out_xsize = block_params[["nXSize"]],
      out_ysize = block_params[["nYSize"]]
    )
  ))
}
#' @title Block writer function
#' @description Writes a block of data to a GDALRaster dataset.
#' @param ds The GDALRaster dataset object to write to.
#' @param bps A list of block parameters including band number and offsets.
#' @param data A matrix of raster data to write.
#' @return NULL
#' @keywords internal
#' @noRd
blockwriter <- function(ds, bps, data) {
  ds$write(
    band = bps[["band_n"]],
    xoff = bps[["nXOff"]],
    yoff = bps[["nYOff"]],
    xsize = bps[["nXSize"]],
    ysize = bps[["nYSize"]],
    rasterData = data
  )
  # ds$flushCache() # I think we want this to clear  memory as we go...
  return(invisible())
}


#' @title Asynchronous multi-band reduction read/write
#' @description Reads multi-band data in blocks, applies a reduction function,
#' and writes the results to a GDALRaster dataset. Used internally by
#' `gdalraster::multiband_reduce()`.
#' @param blocks A dataframe with block parameters.
#' @param vrt_file The VRT file path.
#' @param ds The GDALRaster dataset to write to - must be open and with
#' `readonly=FALSE`.
#' @param raster_template A template raster object containing metadata.
#' @param reduce_fun The reduction function to apply to the multi-band data.
#' @return invisible
#' @keywords internal
#' @noRd
async_gdalreader_multiband_reduce_read_write <- function(
  blocks,
  vrt_file,
  ds,
  raster_template,
  reduce_fun
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
    expr = rlang::expr(
      multiband_writer(j, ds, raster_template) # nolint
    ),
    msg = "mirai GDAL multi-band reducer error"
  )
  return(invisible())
}

#' @title Sequential multi-band reduction read/write
#' @description Reads multi-band data in blocks, applies a reduction function,
#' and writes the results to a GDALRaster dataset. Used internally by
#' `gdalraster::multiband_reduce()`.
#' @param blocks A dataframe with block parameters.
#' @param vrt_file The VRT file path.
#' @param ds The GDALRaster dataset to write to - must be open and with
#' `readonly=FALSE`.
#' @param raster_template A template raster object containing metadata.
#' @param reduce_fun The reduction function to apply to the multi-band data.
#' @param quiet Logical, if TRUE suppresses progress messages.
#' @return invisible
#' @keywords internal
#' @noRd
sequential_gdalreader_multiband_reduce_read_write <- function(
  blocks,
  vrt_file,
  ds,
  raster_template,
  reduce_fun,
  quiet
) {
  purrr::pwalk(
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

      multiband_writer(
        list(
          data = restructure_cells(cell_vals),
          block = block_params
        ),
        ds,
        raster_template
      )
    },
    .progress = !quiet
  )
  return(invisible())
}


#' @title Multi-band writer function
#' @description Writes multi-band data to a GDALRaster dataset.
#' @param j A list containing the block data and parameters.
#' @param ds The GDALRaster dataset to write to.
#' @param raster_template A template raster object containing metadata.
#' @return NULL
#' @keywords internal
#' @noRd
multiband_writer <- function(j, ds, raster_template) {
  n_cells <- j$block$nXSize * j$block$nYSize
  band_starts <- seq(1, n_cells * raster_template$nbands, by = n_cells)
  band_ends <- band_starts + n_cells - 1

  j$data[is.na(j$data)] <- raster_template$nodataval

  purrr::walk(seq_len(raster_template$nbands), function(b) {
    start_idx <- band_starts[b]
    end_idx <- band_ends[b]
    band_data <- j$data[start_idx:end_idx]

    blockwriter(ds, c(band_n = b, j$block), band_data)
  })
  return(invisible())
}

#' @title Asynchronous single-band many-to-many read/write
#' @description Reads single-band data in blocks, applies a many-to-many function,
#' and writes the results to a GDALRaster dataset. Used internally by
#' `gdalraster::singleband_m2m()`.
#' @param blocks A dataframe with block parameters.
#' @param vrt_collection A list of VRT files for each band.
#' @param ds_list A list of GDALRaster datasets to write to.
#' @param m2m_fun The many-to-many function to apply to the single-band
#' data.
#' @param scale_values A vector of scale values for each band.
#' @return invisible
#' @keywords internal
#' @noRd
async_gdalreader_singleband_m2m_read_write <- function(
  blocks,
  vrt_collection,
  ds_list,
  m2m_fun
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
    compute_with_py_env = compute_with_py_env,
    m2m_fun = m2m_fun,
    matrix_to_rowlist = matrix_to_rowlist
  )

  mirai_async_result_handler(
    jobs,
    ds = ds_list,
    blocks = blocks,
    expr = rlang::expr(
      singleband_m2m_writer(ds, j) # nolint
    ),
    msg = "mirai GDAL single-band many-to-many error"
  )
  return(invisible())
}

#' @title Sequential single-band many-to-many read/write
#' @description Reads single-band data in blocks, applies a many-to-many function,
#' and writes the results to a GDALRaster dataset. Used internally by
#' `gdalraster::singleband_m2m()`.
#' @param blocks A dataframe with block parameters.
#' @param vrt_collection A list of VRT files for each band.
#' @param ds_list A list of GDALRaster datasets to write to.
#' @param m2m_fun The many-to-many function to apply to the single-band
#' data.
#' @param scale_values A vector of scale values for each band.
#' @param quiet Logical, if TRUE suppresses progress messages.
#' @return invisible
#' @keywords internal
#' @noRd
sequential_gdalreader_singleband_m2m_read_write <- function(
  blocks,
  vrt_collection,
  ds_list,
  m2m_fun,
  quiet
) {
  purrr::pwalk(
    blocks,
    function(...) {
      block_params <- rlang::dots_list(...)
      band_data <- purrr::map(
        vrt_collection[[1]],
        ~ single_band_reader()(.x, block_params)
      )

      bdm <- do.call(rbind, band_data)
      hamp_bdm <- m2m_fun(bdm)

      singleband_m2m_writer(
        ds_list = ds_list,
        j = list(
          band_data = matrix_to_rowlist(hamp_bdm),
          block_params = block_params
        )
      )
    },
    .progress = !quiet
  )

  return(invisible())
}

#' @title Single-band reader function
#' @description Writes data to multiple GDALRaster datasets.
#' @param ds_list A list of GDALRaster datasets to write to.
#' @param j A list containing the block data and parameters.
#' @return NULL
#' @keywords internal
#' @noRd
singleband_m2m_writer <- function(ds_list, j) {
  .params <- j$block_params
  purrr::pwalk(
    list(.ds = ds_list, .data = j$band_data),
    function(.ds, .data) {
      # Write the band data to the output raster
      blockwriter(.ds, .params, .data)
    }
  )
  return(invisible())
}
