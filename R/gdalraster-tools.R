#' Generate params from a raster file
#' @param x character path to the raster file
#' @param temp_vrt_dir character path to the temporary VRT directory
#' @param apply_scale logical indicating whether to apply the scale values
#' (if they exist) to the data
#' @return list containing the raster parameters
#' @keywords internal
#' @noRd
raster_template_params <- function(
  x,
  temp_vrt_dir = getOption("vrt.cache"),
  apply_scale = TRUE
) {
  vrt_template <- x$vrt_src
  tds <- methods::new(gdalraster::GDALRaster, vrt_template)
  on.exit(tds$close(), add = TRUE)
  xs <- tds$getRasterXSize()
  ys <- tds$getRasterYSize()
  nodataval <- tds$getNoDataValue(1)
  blksize <- tds$getBlockSize(1)
  nbands <- tds$getRasterCount()
  scale_vals <- purrr::map_vec(
    seq_len(nbands),
    ~ tds$getScale(.x)
  )
  offset_vals <- purrr::map_vec(
    seq_len(nbands),
    ~ tds$getOffset(.x)
  )
  data_type <- tds$getDataTypeName(1)

  return(list(
    vrt_template = vrt_template,
    xs = xs,
    ys = ys,
    nodataval = nodataval,
    blksize = blksize,
    nbands = nbands,
    scale_vals = scale_vals,
    data_type = data_type,
    offset_vals = offset_vals
  ))
}

#' Assertions for gdalraster engine functions.
#' @keywords internal
#' @noRd
gdalraster_engine_asserts_init <- function(
  outfile,
  config_options,
  creation_options,
  quiet,
  fname = "multiband_reduce"
) {
  v_assert_type(outfile, "outfile", "character", nullok = FALSE)
  v_assert_type(config_options, "config_options", "character", nullok = FALSE)
  v_assert_type(
    creation_options,
    "creation_options",
    "character",
    nullok = FALSE
  )
  v_assert_type(quiet, "quiet", "logical", nullok = FALSE)
}

#' Build a VSI source string for GDAL raster drivers
#' @param src character source path
#' @param vsi character VSI prefix (e.g., "/vsicurl/")
#' @param drive character GDAL driver prefix (e.g., "EOPFZARR")
#' @return character VSI source string
#' @keywords internal
#' @noRd
gdal_driver_vsi_src_builder <- function(src, vsi = "", drive = "") {
  osrc <- paste0(vsi, src)
  if (drive == "EOPFZARR") {
    parts <- strsplit(
      osrc,
      split = "(?<=\\.zarr)",
      perl = TRUE
    )
    osrc <- sapply(parts, function(x) {
      paste0(drive, ":", '"', x[1], '":', x[2])
    })
  } else if (nzchar(drive)) {
    osrc <- paste0(drive, ":", '"', osrc, '"')
  }

  return(osrc)
}


vals_to_array <- function(r, dims = attributes(r)$gis$dim) {
  rows <- dims[1]
  cols <- dims[2]

  # Auto-determine number of bands
  pixels_per_band <- rows * cols
  total_length <- length(r)

  if (total_length %% pixels_per_band != 0) {
    cli::cli_abort(
      "Data length is not a multiple of spatial dimensions 
      {rows * cols}. Cannot reshape to array.",
      class = "invalid_data_length_error"
    )
  }

  nbands <- total_length / pixels_per_band

  array(r, dim = c(rows, cols, nbands))
}


#' Get appropriate NoData value for GDAL data type
#' @param data_type character GDAL data type name
#' @return numeric NoData value appropriate for the data type
#' @keywords internal
#' @noRd
get_nodata_value <- function(data_type) {
  switch(
    data_type,
    "Byte" = 255, # Max value for unsigned byte
    "Int8" = -128, # Min value for signed byte
    "UInt16" = 65535, # Max value for unsigned 16-bit
    "Int16" = -32768, # Min value for signed 16-bit
    "UInt32" = 4294967295, # Max value for unsigned 32-bit
    "Int32" = -2147483648, # Min value for signed 32-bit
    "Float32" = NA_real_, # NaN for 32-bit float
    "Float64" = NA_real_, # NaN for 64-bit float
    "CInt16" = -32768, # Complex types use min int
    "CInt32" = -2147483648,
    "CFloat32" = NA_real_,
    "CFloat64" = NA_real_,
    0 # Default fallback
  )
}
