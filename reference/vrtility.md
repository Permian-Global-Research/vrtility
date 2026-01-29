# vrtility: GDAL VRT utilities for R

vrtility leverages GDAL's VRT (Virtual Raster) capabilities to build
efficient raster processing pipelines, with a focus on Earth Observation
applications. The package uses a modular design based on nested VRTs,
enabling complex image processing tasks such as cloud masking,
multi-temporal compositing, and time series filtering.

Key features include:

- Cloud masking via VRT pixel functions (Python/NumPy or GDAL/muparser)

- Multi-band compositing methods (median, geomedian, medoid) that
  maintain spectral consistency

- Time series filtering (e.g. Hampel filter) for temporal noise
  reduction

- Efficient parallel processing via
  [mirai](https://mirai.r-lib.org/index.html)

- Direct integration with STAC catalogs for cloud-native workflows

Powered by [gdalraster](https://firelab.github.io/gdalraster/).

## VRT Core Functions

- [`vrt_collect`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md):

  Create a vrt_collection object from STAC items or file paths

- [`vrt_plan`](https://permian-global-research.github.io/vrtility/reference/vrt_plan.md):

  Create a lightweight metadata plan from STAC items (for optimized
  warping)

- [`vrt_warp`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md):

  Warp a vrt_x object to a common grid

- [`vrt_stack`](https://permian-global-research.github.io/vrtility/reference/vrt_stack.md):

  Create a vrt_stack object from a warped collection

- [`vrt_compute`](https://permian-global-research.github.io/vrtility/reference/vrt_compute.md):

  Compute a vrt pipeline (materialize to disk)

## VRT Pixel Functions

- [`vrt_set_gdal_pixelfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_gdal_pixelfun.md):

  Set a GDAL built-in pixel function for a vrt stack object

- [`vrt_set_py_pixelfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md):

  Set a Python pixel function for a vrt stack object

- [`vrt_derived_block`](https://permian-global-research.github.io/vrtility/reference/vrt_derived_block.md):

  Create derived bands using muparser expressions

## Image Masking

- [`vrt_set_maskfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  Set a mask function for a vrt object

- [`vrt_create_mask`](https://permian-global-research.github.io/vrtility/reference/vrt_create_mask.md):

  Create a mask for a vrt object

- [`build_intmask`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  Build a mask from integer values (auto-selects muparser or Python)

- [`build_bitmask`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  Build a mask from bit positions (auto-selects muparser or Python)

- [`create_omnicloudmask`](https://permian-global-research.github.io/vrtility/reference/vrt_create_mask.md):

  Create a cloud mask using the omnicloudmask algorithm

## VRT Helpers

- [`vrt_set_band_names`](https://permian-global-research.github.io/vrtility/reference/vrt_set_band_names.md):

  Set band names for a vrt_x object

- [`vrt_save`](https://permian-global-research.github.io/vrtility/reference/vrt_save.md):

  Save a VRT object to a file

- [`vrt_add_empty_band`](https://permian-global-research.github.io/vrtility/reference/vrt_add_empty_band.md):

  Add an empty band to a vrt_x object

- [`vrt_move_band`](https://permian-global-research.github.io/vrtility/reference/vrt_move_band.md):

  Move a band in a vrt_x object

- [`vrt_set_scale`](https://permian-global-research.github.io/vrtility/reference/vrt_set_scale.md):

  Set the scale and offset values for a vrt_x object

- [`vrt_set_nodata`](https://permian-global-research.github.io/vrtility/reference/vrt_set_nodata.md):

  Set the nodata value for a vrt_x object

- [`vrt_schema`](https://permian-global-research.github.io/vrtility/reference/vrt_tools.md):

  (data object) The official GDAL VRT schema

## Multi-band Reduction Functions

- [`multiband_reduce`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md):

  Create composite reductions that require all bands

- [`geomedian`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md):

  A geometric median reducer function

- [`medoid`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md):

  A medoid reducer function

- [`geomedoid`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md):

  A geometric medoid reducer function

- [`quantoid`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md):

  A quantoid reducer function

## Single-band Many-to-Many Functions

- [`singleband_m2m`](https://permian-global-research.github.io/vrtility/reference/singleband-many-to-many.md):

  Apply single-band many-to-many operations (e.g. outlier filtering)

- [`hampel_filter`](https://permian-global-research.github.io/vrtility/reference/singleband-many-to-many.md):

  Hampel filter for outlier detection

## GDAL Configuration

- [`gdal_config_options`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Set GDAL configuration options for HTTP, caching, etc.

- [`set_gdal_config`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Apply GDAL configuration options

- [`gdal_creation_options`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Set GDAL creation options for output files

- [`gdalwarp_options`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Set gdalwarp-specific options

- [`set_gdal_cache_max`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Set GDAL cache size as a fraction of system RAM

- [`gdal_raster_drivers`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  List available GDAL raster drivers

- [`check_muparser`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md):

  Check if GDAL muparser support is available

## Python Integration

- [`compute_with_py_env`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md):

  Execute code with vrtility Python environment

- [`set_py_env_vals`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md):

  Set Python environment variables as options

- [`vrtility_py_require`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md):

  Require Python packages for vrtility

- [`median_numpy`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md):

  A Python pixel function to compute the median

## STAC Functions

- [`stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog

- [`sentinel2_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for Sentinel-2 data

- [`hls_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for HLS data

- [`sentinel1_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for Sentinel-1 data

- [`stac_cloud_filter`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Filter a STAC collection for cloud cover

- [`stac_orbit_filter`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Filter a STAC collection for orbit state

- [`sign_mpc_items`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Sign STAC items from Microsoft Planetary Computer

## Raster Visualisation

- [`plot_raster_src`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md):

  Plot a raster source (file path or VRT)

- [`plot.vrt_block`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md):

  Plot method for vrt_block objects

- [`plot.vrt_collection`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md):

  Plot method for vrt_collection objects

## Cache Management

- [`vrt_cache_destroy`](https://permian-global-research.github.io/vrtility/reference/vrt_cache_management.md):

  Destroy the vrtility cache directory

- [`vrt_cache_set`](https://permian-global-research.github.io/vrtility/reference/vrt_cache_management.md):

  Set the vrtility cache directory

## Parallel Processing

- [`n_daemons`](https://permian-global-research.github.io/vrtility/reference/mirai-mgmt.md):

  Get the number of mirai daemons running

- [`daemons_load_vrtility`](https://permian-global-research.github.io/vrtility/reference/mirai-mgmt.md):

  Load vrtility Python environment in mirai daemons

## Spatial Helpers

- [`bbox_to_projected`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Convert a long/lat bounding box to a projected bounding box

- [`ogr_bbox_from_file`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Get a bounding box from a vector file

- [`ogr_srs_from_file`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Get a spatial reference system from a vector file

## gdalraster Helpers

- [`r_to_MEM`](https://permian-global-research.github.io/vrtility/reference/gdalraster-helpers.md):

  Convert a vector to a GDALRasterDataset in memory

## See also

[LPCLOUD STAC
Browser](https://radiantearth.github.io/stac-browser/#/external/cmr.earthdata.nasa.gov/stac/LPCLOUD/)

## Author

**Maintainer**: Hugh Graham <hugh.graham@permianglobal.com>
([ORCID](https://orcid.org/0000-0001-9451-5010))
