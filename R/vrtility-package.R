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
#' \item{\code{\link{ogr_bbox_from_file}}}{Get a bounding box from a vector file}
#' \item{\code{\link{ogr_srs_from_file}}}{Get a spatial reference system from a vector file}
#' }
#' @section STAC helpers:
#' \describe{
#' \item{\code{\link{stac_query}}}{Query a STAC catalog}
#' \item{\code{\link{sentinel2_stac_query}}}{Query a STAC catalog for Sentinel-2 data}
#' \item{\code{\link{hls_stac_query}}}{Query a STAC catalog for HLS data}
#' \item{\code{\link{stac_cloud_filter}}}{Filter a STAC collection for cloud cover}
#' \item{\code{\link{sentinel1_stac_query}}}{Query a STAC catalog for Sentinel-1 data}
#' \item{\code{\link{stac_orbit_filter}}}{Filter a STAC collection for orbit state}
#' \item{\code{\link{sign_mpc_items}}}{Sign STAC items from Microsoft Planetary Computer}
#' }
#' @section VRT utilities:
#' \describe{
#' \item{\code{\link{vrt_collect}}}{Create a vrt_collection object}
#' \item{\code{\link{vrt_set_maskfun}}}{Set a mask function for a vrt object}
#' \item{\code{\link{vrt_create_mask}}}{Create a mask for a vrt object}
#' \item{\code{\link{vrt_stack}}}{Create a vrt_stack object}
#' \item{\code{\link{vrt_set_py_pixelfun}}}{Set a pixel function for a vrt stack object}
#' \item{\code{\link{vrt_set_gdal_pixelfun}}}{Set a GDAL pixel function for a vrt stack object}
#' \item{\code{\link{vrt_set_scale}}}{Set the scale and offset values for a vrt_x object}
#' \item{\code{\link{vrt_move_band}}}{Move a band in a vrt_x object}
#' \item{\code{\link{vrt_set_nodata}}}{Set the nodata value for a vrt_x object}
#' \item{\code{\link{vrt_warp}}}{Warp a vrt_x object to a warped vrt}
#' \item{\code{\link{vrt_compute}}}{Compute a vrt pipeline (using GDAL)}
#' }
#' @section VRT pixel functions:
#' \describe{
#' \item{\code{\link{vrt_set_py_pixelfun}}}{Set a pixel function for a vrt stack object}
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
#' @section gdalraster helpers:
#' \describe{
#' \item{\code{\link{r_to_MEM}}}{Convert a vector to a GDALRasterDataset in memory}
#' }
#' @section Composite reducers:
#' \describe{
#' \item{\code{\link{multiband_reduce}}}{Create composite reductions that require all bands}
#' \item{\code{\link{geomedian}}}{A geometric median reducer function}
#' \item{\code{\link{medoid}}}{A medoid reducer function}
#' \item{\code{\link{geomedoid}}}{A geometric medoid reducer function}
#' \item{\code{\link{quantoid}}}{A quantoid reducer function}
#' }
#' @seealso
#' \href{https://radiantearth.github.io/stac-browser/#/external/cmr.earthdata.nasa.gov/stac/LPCLOUD/}{LPCLOUD STAC Browser}
#'
#' @docType package
#' @aliases vrtility-package
#' @name vrtility
#' @keywords internal
"_PACKAGE"


.onLoad <- function(libname, pkgname) {
  # TODO: do we still need this? Check latest reticulate advice.
  if (is.na(Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA))) {
    Sys.setenv(RETICULATE_USE_MANAGED_VENV = "yes")
  }
  vrtility_py_require("numpy")
  cache_init_checks()
  vrt_opts_set()

  check_gdal_and_warn()
  set_gdal_cache_max()
}

#' onload max RAM allocation options
#' @noRd
#' @keywords internal
vrt_opts_set <- function() {
  op <- options()
  op_vrtility <- list(
    vrt.percent.ram = 60,
    vrt.pause.base = 1,
    vrt.pause.cap = 10,
    vrt.max.times = 3,
    vrtility.use_muparser = FALSE
  )

  toset <- !(names(op_vrtility) %in% names(op))
  if (any(toset)) options(op_vrtility[toset])
}

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib vrtility, .registration = TRUE
## usethis namespace: end
NULL
