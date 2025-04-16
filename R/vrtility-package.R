#' @title vrtility: GDAL VRT utilities for R
#'
#' @description vrtility is a package for building raster (primarily remote sensing)
#' processing pipelines. It makes use of GDAL's VRT (virtual raster format)
#' capabilities for efficient processing of large raster datasets. This
#' package's primary focus is  on the use of GDAL VRT pixel functions. These
#' pixel functions (currently implemented with python) are used to apply
#' cloud masks and summarise pixel values (e.g. median) from multiple images
#' (i.e create a composite image). We hope to add C++ or expression based
#' pixel functions in time.
#' @section Spatial helpers:
#' \describe{
#'  \item{\code{\link{bbox_to_projected}}}{Convert a long/lat bounding box to a projected bounding box}
#' \item{\code{\link{to_wkt}}}{Get a wkt string from a numeric epsg code or character such as a proj4 string}
#' }
#' @section STAC helpers:
#' \describe{
#' \item{\code{\link{stac_query}}}{Query a STAC catalog}
#' \item{\code{\link{sentinel2_stac_query}}}{Query a STAC catalog for Sentinel-2 data}
#' \item{\code{\link{sign_planetary_computer}}}{Sign a URL for the Planetary Computer}
#' }
#' @section VRT utilities:
#' \describe{
#' \item{\code{\link{vrt_collect}}}{Create a vrt_collection object}
#' \item{\code{\link{vrt_set_maskfun}}}{Set a mask function for a vrt object}
#' \item{\code{\link{vrt_stack}}}{Create a vrt_stack object}
#' \item{\code{\link{vrt_set_pixelfun}}}{Set a pixel function for a vrt stack object}
#' \item{\code{\link{vrt_warp}}}{Warp a vrt_x object to a warped vrt}
#' \item{\code{\link{vrt_compute}}}{Compute a vrt pipeline (using GDAL)}
#' }
#' @section VRT pixel functions:
#' \describe{
#' \item{\code{\link{vrt_set_pixelfun}}}{Set a pixel function for a vrt stack object}
#' \item{\code{\link{set_mask_numpy}}}{A pixel function to apply a bitmask}
#' \item{\code{\link{build_intmask}}}{A pixel function to build a mask from a mask band where the mask values are integers}
#' \item{\code{\link{build_bitmask}}}{A pixel function to build a mask from a mask band where the mask values are bit positions}
#' \item{\code{\link{median_numpy}}}{A pixel function to compute the median}
#' }
#' @section python environment helpers:
#' \describe{
#' \item{\code{\link{vrtility_py_require}}}{Require a python package}
#' \item{\code{\link{set_py_env_vals}}}{Set python environment variables as options}
#' }
#' @section VRT helpers:
#' \describe{
#' \item{\code{\link{vrt_save}}}{Save a VRT object to a file}
#' \item{\code{\link{vrt_schema}}}{(data object)The official GDAL VRT schema as a character object}
#' }
#'
#' @seealso
#' \href{https://radiantearth.github.io/stac-browser/#/external/cmr.earthdata.nasa.gov/stac/LPCLOUD/}{LPCLOUD STAC Browser}
#'
#' @docType package
#' @aliases vrtility-package
#' @name vrtility
#' @keywords internal
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  if (is.na(Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA))) {
    Sys.setenv(RETICULATE_USE_MANAGED_VENV = "yes")
  }
  vrtility_py_require(c(
    "numpy"
  ))
  cache_init_checks()
  vrt_ram_opts_set()
}

#' onload max RAM allocation options
#' @noRd
#' @keywords internal
vrt_ram_opts_set <- function() {
  op <- options()
  op_vrtility <- list(
    vrt.percent.ram = 60
  )

  toset <- !(names(op_vrtility) %in% names(op))
  if (any(toset)) options(op_vrtility[toset])
}

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib vrtility, .registration = TRUE
## usethis namespace: end
NULL
