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
  vrt_template <- vrt_block_save_internal(
    x,
    temp_vrt_dir = temp_vrt_dir,
    apply_scale = apply_scale
  )
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
