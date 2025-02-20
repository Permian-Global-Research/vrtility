#' Generate a composite raster from (virtual) raster sources.
#' @param src_files A stac_vrt object or a character vector of file paths to the
#' rasters
#' @param outfile A character string of the output file path
#' @param bbox A numeric vector of the bounding box (length 4) in lat/long
#' @param fun A character string of the pixel function to apply (only median is
#' supported for now...)
#' @param t_srs A character string of the target SRS
#' @param res A numeric of the target resolution in units of t_srs
#' @param vrt_options A list of options to pass to the VRT
#' @param warp_options A character vector of options to pass to the warp
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
#' @export
#' @details This is the main package function at present. It is quite targetted
#' towards Sentinel 2 data and will likely change - but this will server as the
#' main testing point for now!
vrt_composite <- function(
    src_files, outfile, bbox,
    fun = numba_median,
    t_srs = to_generic_projected(bbox),
    res = 10,
    vrt_options = NULL,
    warp_options = c(
      "-overwrite",
      "-te", gdalraster::bbox_transform(
        bbox,
        gdalraster::srs_to_wkt("EPSG:4326"),
        to_generic_projected(bbox)
      ),
      "-tr", res, res,
      "-tap",
      "-r", "bilinear",
      "-co", "COMPRESS=DEFLATE",
      "-co", "PREDICTOR=2",
      "-co", "NUM_THREADS=ALL_CPUS"
    ),
    config_options = c(
      GDAL_VRT_ENABLE_PYTHON = "YES",
      VSI_CACHE = "TRUE",
      GDAL_CACHEMAX = "30%",
      VSI_CACHE_SIZE = "10000000",
      GDAL_HTTP_MULTIPLEX = "YES",
      GDAL_INGESTED_BYTES_AT_OPEN = "32000",
      GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
      GDAL_HTTP_VERSION = "2",
      GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
      GDAL_NUM_THREADS = "ALL_CPUS"
    ),
    quiet = FALSE) {
  # First, ensure we have the correct paths
  py_bin <- Sys.getenv("VRTILITY_PY_EXECUTABLE", unset = NA)
  if (is.na(py_bin)) {
    cli::cli_abort(
      c("Cannot locate the {cli::style_bold('VRTILITY_PYTHON')} environment",
        "i" = "You may need to run {cli::code_highlight('`build_vrtility_python()`')} to install it"
      )
    )
  }

  py_env <- dirname(dirname(py_bin))

  # Modified environment setup
  withr::with_envvar(
    new = c(
      # Only set essential variables
      "RETICULATE_PYTHON" = py_bin,
      "VIRTUALENV" = py_env,
      "PATH" = paste(
        fs::path(py_env, "bin"),
        Sys.getenv("PATH"),
        sep = ":"
      )
    ),
    code = {
      # Initialize reticulate first
      reticulate::use_python(py_bin, required = TRUE)

      call_vrt_composite(
        src_files = src_files,
        outfile = outfile,
        fun = fun,
        t_srs = t_srs,
        warp_options = warp_options
      )
    }
  )
}



#' Generate a composite raster from (virtual) raster sources.
#' @param src_files A stac_vrt object or a character vector of file paths to the
#' rasters
#' @param outfile A character string of the output file path
#' @param fun A character string of the pixel function to apply (only median is
#' supported for now...)
#' @param t_srs A character string of the target SRS
#' @param vrt_options A list of options to pass to the VRT
#' @param warp_options A character vector of options to pass to the warp
#' @param config_options A character vector of options to set in the GDAL
#' environment
#' @param quiet A logical indicating whether to suppress output
#' @return A character string of the path to the output raster
#' @details This function will likely fail unless the python interpreter has
#' been set locally. Insead its best to use the `vrt_composite` function which
#' will handle this for you.
#' @keywords internal
#' @noRd
call_vrt_composite <- function(
    src_files,
    outfile,
    fun = numba_median,
    t_srs = "",
    vrt_options = NULL,
    warp_options = c(
      "-r", "near",
      "-co", "COMPRESS=DEFLATE",
      "-co", "PREDICTOR=2",
      "-co", "NUM_THREADS=10"
    ),
    config_options = c(
      GDAL_VRT_ENABLE_PYTHON = "YES",
      VSI_CACHE = "TRUE",
      GDAL_CACHEMAX = "30%",
      VSI_CACHE_SIZE = "10000000",
      GDAL_HTTP_MULTIPLEX = "YES",
      GDAL_INGESTED_BYTES_AT_OPEN = "32000",
      GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
      GDAL_HTTP_VERSION = "2",
      GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
      GDAL_NUM_THREADS = "ALL_CPUS"
      # , CPL_DEBUG = "ON"
    ),
    quiet = FALSE) {
  if (inherits(src_files, "stac_vrt")) {
    vrt_xml <- xml2::read_xml(src_files$vrt)
    src_files <- fs::file_temp(ext = "vrt")
    xml2::write_xml(vrt_xml, src_files)
  }
  # TODO: this wont warn about stac_vrt so add at some point.
  v_assert_type(src_files, "src_files", "character")
  v_assert_type(outfile, "outfile", "character")
  v_assert_type(fun, "fun", "function")
  v_assert_type(t_srs, "t_srs", "character")
  v_assert_type(vrt_options, "vrt_options", "character")
  v_assert_type(warp_options, "warp_options", "character")
  v_assert_type(config_options, "config_options", "character")

  purrr::iwalk(
    config_options,
    ~ gdalraster::set_config_option(.y, .x)
  )


  pix_fun_vrt <- vrt_pixfun(src_files, vrt_options, fun())

  gdalraster::warp(
    pix_fun_vrt,
    outfile,
    t_srs = t_srs,
    cl_arg = warp_options,
    quiet = quiet
  )

  return(outfile)
}
