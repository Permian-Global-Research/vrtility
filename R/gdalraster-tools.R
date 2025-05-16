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
  data_type <- tds$getDataTypeName(1)
  if (any(!is.na(scale_vals))) {
    data_type <- "Float32"
  }

  return(list(
    vrt_template = vrt_template,
    xs = xs,
    ys = ys,
    nodataval = nodataval,
    blksize = blksize,
    nbands = nbands,
    scale_vals = scale_vals,
    data_type = data_type
  ))
}

#' Assertions for gdalraster engine functions.
#' @keywords internal
#' @noRd
gdalraster_engine_asserts_init <- function(
  outfile,
  cache_dir,
  config_options,
  creation_options,
  quiet,
  fname = "multiband_reduce"
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
  v_assert_type(quiet, "quiet", "logical", nullok = FALSE)

  if (!using_daemons()) {
    cli::cli_inform(
      c(
        "!" = "No miriai daemons detected.",
        " " = "use {cli::code_highlight('mirai::daemons()')} to start daemons",
        "i" = "`{fname}` is a compute-intesntive function and is
        designed to be run in parallel"
      )
    )
  }
}
