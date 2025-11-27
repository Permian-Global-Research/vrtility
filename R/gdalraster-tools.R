#' Generate params from a raster file
#' @param x character path to the raster file
#' @return list containing the raster parameters
#' @keywords internal
#' @noRd
raster_template_params <- function(
  x
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

  tds$close()

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

#' get source block size from raster file
#' @param src character path to raster file
#' @param band numeric band number
#' @return numeric vector of block size c(xsize, ysize)
#' @keywords internal
#' @noRd
src_block_size <- function(
  src,
  band = 1,
  co_format = TRUE,
  mingdal = gdalraster::gdal_compute_version(3, 10, 0)
) {
  #TODO: consider if we must force blocksizes of a multiple of 16
  b1ds <- methods::new(gdalraster::GDALRaster, src)
  on.exit(b1ds$close(), add = TRUE)
  blksize <- b1ds$getBlockSize(band)
  b1ds$close()

  if (co_format) {
    if (mingdal <= gdalraster::gdal_version_num()) {
      blksize <- c(
        "-co",
        glue::glue("BLOCKXSIZE={blksize[1]}"),
        "-co",
        glue::glue("BLOCKYSIZE={blksize[2]}")
      )
    } else {
      blksize <- NULL
    }
  }

  return(blksize)
}

#' get source metadata from raster file
#' @param src character path to raster file
#' @return named character vector of metadata items
#' @keywords internal
#' @noRd
read_src_metadata <- function(src) {
  tds <- methods::new(gdalraster::GDALRaster, src)
  on.exit(tds$close(), add = TRUE)
  tmp_metadata <- tds$getMetadata(0, "")
  tds$close()
  mtd_name_splt <- strsplit(tmp_metadata, "=")

  purrr::set_names(
    purrr::map_chr(mtd_name_splt, function(x) x[2]),
    purrr::map_chr(mtd_name_splt, function(x) x[1])
  )
}
