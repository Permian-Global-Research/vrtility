#' @title vrtility: GDAL VRT utilities for R
#'
#' @description vrtility leverages GDAL's VRT (Virtual Raster) capabilities to
#' build efficient raster processing pipelines, with a focus on Earth Observation
#' applications. The package uses a modular design based on nested VRTs, enabling
#' complex image processing tasks such as cloud masking, multi-temporal compositing,
#' and time series filtering.
#'
#' Key features include:
#' \itemize{
#'   \item Cloud masking via VRT pixel functions (Python/NumPy or GDAL/muparser)
#'   \item Multi-band compositing methods (median, geomedian, medoid) that
#'     maintain spectral consistency
#'   \item Time series filtering (e.g. Hampel filter) for temporal noise reduction
#'   \item Efficient parallel processing via \href{https://mirai.r-lib.org/index.html}{mirai}
#'   \item Direct integration with STAC catalogs for cloud-native workflows
#' }
#'
#' Powered by \href{https://firelab.github.io/gdalraster/}{gdalraster}.
#'
#' @section VRT Core Functions:
#' \describe{
#' \item{\code{\link{vrt_collect}}}{Create a vrt_collection object from STAC items or file paths}
#' \item{\code{\link{vrt_plan}}}{Create a lightweight metadata plan from STAC items (for optimized warping)}
#' \item{\code{\link{vrt_warp}}}{Warp a vrt_x object to a common grid}
#' \item{\code{\link{vrt_stack}}}{Create a vrt_stack object from a warped collection}
#' \item{\code{\link{vrt_compute}}}{Compute a vrt pipeline (materialize to disk)}
#' }
#' @section VRT Pixel Functions:
#' \describe{
#' \item{\code{\link{vrt_set_gdal_pixelfun}}}{Set a GDAL built-in pixel function for a vrt stack object}
#' \item{\code{\link{vrt_set_py_pixelfun}}}{Set a Python pixel function for a vrt stack object}
#' \item{\code{\link{vrt_derived_block}}}{Create derived bands using muparser expressions}
#' }
#' @section Image Masking:
#' \describe{
#' \item{\code{\link{vrt_set_maskfun}}}{Set a mask function for a vrt object}
#' \item{\code{\link{vrt_create_mask}}}{Create a mask for a vrt object}
#' \item{\code{\link{build_intmask}}}{Build a mask from integer values (auto-selects muparser or Python)}
#' \item{\code{\link{build_bitmask}}}{Build a mask from bit positions (auto-selects muparser or Python)}
#' \item{\code{\link{create_omnicloudmask}}}{Create a cloud mask using the omnicloudmask algorithm}
#' }
#' @section VRT Helpers:
#' \describe{
#' \item{\code{\link{vrt_set_band_names}}}{Set band names for a vrt_x object}
#' \item{\code{\link{vrt_save}}}{Save a VRT object to a file}
#' \item{\code{\link{vrt_add_empty_band}}}{Add an empty band to a vrt_x object}
#' \item{\code{\link{vrt_move_band}}}{Move a band in a vrt_x object}
#' \item{\code{\link{vrt_set_scale}}}{Set the scale and offset values for a vrt_x object}
#' \item{\code{\link{vrt_set_nodata}}}{Set the nodata value for a vrt_x object}
#' \item{\code{\link{vrt_schema}}}{(data object) The official GDAL VRT schema}
#' }
#' @section Multi-band Reduction Functions:
#' \describe{
#' \item{\code{\link{multiband_reduce}}}{Create composite reductions that require all bands}
#' \item{\code{\link{geomedian}}}{A geometric median reducer function}
#' \item{\code{\link{medoid}}}{A medoid reducer function}
#' \item{\code{\link{geomedoid}}}{A geometric medoid reducer function}
#' \item{\code{\link{quantoid}}}{A quantoid reducer function}
#' }
#' @section Single-band Many-to-Many Functions:
#' \describe{
#' \item{\code{\link{singleband_m2m}}}{Apply single-band many-to-many operations (e.g. outlier filtering)}
#' \item{\code{\link{hampel_filter}}}{Hampel filter for outlier detection}
#' }
#' @section GDAL Configuration:
#' \describe{
#' \item{\code{\link{gdal_config_options}}}{Set GDAL configuration options for HTTP, caching, etc.}
#' \item{\code{\link{set_gdal_config}}}{Apply GDAL configuration options}
#' \item{\code{\link{gdal_creation_options}}}{Set GDAL creation options for output files}
#' \item{\code{\link{gdalwarp_options}}}{Set gdalwarp-specific options}
#' \item{\code{\link{set_gdal_cache_max}}}{Set GDAL cache size as a fraction of system RAM}
#' \item{\code{\link{gdal_raster_drivers}}}{List available GDAL raster drivers}
#' \item{\code{\link{check_muparser}}}{Check if GDAL muparser support is available}
#' }
#' @section Python Integration:
#' \describe{
#' \item{\code{\link{compute_with_py_env}}}{Execute code with vrtility Python environment}
#' \item{\code{\link{set_py_env_vals}}}{Set Python environment variables as options}
#' \item{\code{\link{vrtility_py_require}}}{Require Python packages for vrtility}
#' \item{\code{\link{median_numpy}}}{A Python pixel function to compute the median}
#' }
#' @section STAC Functions:
#' \describe{
#' \item{\code{\link{stac_query}}}{Query a STAC catalog}
#' \item{\code{\link{sentinel2_stac_query}}}{Query a STAC catalog for Sentinel-2 data}
#' \item{\code{\link{hls_stac_query}}}{Query a STAC catalog for HLS data}
#' \item{\code{\link{sentinel1_stac_query}}}{Query a STAC catalog for Sentinel-1 data}
#' \item{\code{\link{stac_cloud_filter}}}{Filter a STAC collection for cloud cover}
#' \item{\code{\link{stac_orbit_filter}}}{Filter a STAC collection for orbit state}
#' \item{\code{\link{sign_mpc_items}}}{Sign STAC items from Microsoft Planetary Computer}
#' }
#' @section Raster Visualisation:
#' \describe{
#' \item{\code{\link{plot_raster_src}}}{Plot a raster source (file path or VRT)}
#' \item{\code{\link{plot.vrt_block}}}{Plot method for vrt_block objects}
#' \item{\code{\link{plot.vrt_collection}}}{Plot method for vrt_collection objects}
#' }
#' @section Cache Management:
#' \describe{
#' \item{\code{\link{vrt_cache_destroy}}}{Destroy the vrtility cache directory}
#' \item{\code{\link{vrt_cache_set}}}{Set the vrtility cache directory}
#' }
#' @section Parallel Processing:
#' \describe{
#' \item{\code{\link{n_daemons}}}{Get the number of mirai daemons running}
#' \item{\code{\link{daemons_load_vrtility}}}{Load vrtility Python environment in mirai daemons}
#' }
#' @section Spatial Helpers:
#' \describe{
#' \item{\code{\link{bbox_to_projected}}}{Convert a long/lat bounding box to a projected bounding box}
#' \item{\code{\link{ogr_bbox_from_file}}}{Get a bounding box from a vector file}
#' \item{\code{\link{ogr_srs_from_file}}}{Get a spatial reference system from a vector file}
#' }
#' @section gdalraster Helpers:
#' \describe{
#' \item{\code{\link{r_to_MEM}}}{Convert a vector to a GDALRasterDataset in memory}
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


# Package-level cache environment
.vrtility_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Declare Python dependency (does NOT trigger Python initialization).
  # For numba compatibility e.g. for fastnanquantile;
  # track this issue: https://github.com/numba/numba/issues/10263
  reticulate::py_require("numpy<2.4.0")
  cache_init_checks()
  vrt_opts_set()

  check_gdal_and_warn()
  set_gdal_cache_max()

  # Cache the VRT schema for validation (avoids repeated XML parsing)
  .vrtility_cache$vrt_schema <- xml2::read_xml(vrtility::vrt_xml_schema)
}

#' Get the cached VRT schema
#' @return The cached VRT schema xml_document object
#' @keywords internal
#' @noRd
get_cached_vrt_schema <- function() {
  if (is.null(.vrtility_cache$vrt_schema)) {
    .vrtility_cache$vrt_schema <- xml2::read_xml(vrtility::vrt_xml_schema)
  }
  .vrtility_cache$vrt_schema
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
