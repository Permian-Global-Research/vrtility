# Package index

## VRT Core Functions

Core functions for Virtual Raster management

- [`vrt_collect()`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
  [`print(`*`<vrt_collection>`*`)`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
  [`c(`*`<vrt_block>`*`)`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
  [`c(`*`<vrt_collection>`*`)`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
  : Construct the base VRT object for composing VRT pipelines.
- [`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
  : Construct A warped VRT or warped VRT collection.
- [`vrt_stack()`](https://permian-global-research.github.io/vrtility/reference/vrt_stack.md)
  [`print(`*`<vrt_stack>`*`)`](https://permian-global-research.github.io/vrtility/reference/vrt_stack.md)
  : Stack VRT files from a vrt_collection object
- [`vrt_compute()`](https://permian-global-research.github.io/vrtility/reference/vrt_compute.md)
  : Generate a composite raster from (virtual) raster sources.

## VRT Pixel Functions

Functions for pixel-wise operations on VRT rasters

- [`vrt_set_gdal_pixelfun()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_gdal_pixelfun.md)
  : Set built-in GDAL pixel functions of a VRT stack object
- [`vrt_set_py_pixelfun()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  [`median_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  [`mean_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  [`geomean_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  [`quantile_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  [`mean_db_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md)
  : Set the pixel function of a VRT stack object
- [`vrt_derived_block()`](https://permian-global-research.github.io/vrtility/reference/vrt_derived_block.md)
  : Create vrt blocks with derived bands.

## Image Masking

Functions for creating and applying image masks

- [`vrt_set_maskfun()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
  [`set_mask_numpy()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
  [`build_intmask()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
  [`build_bitmask()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
  : Set mask band of a VRT collection
- [`vrt_create_mask()`](https://permian-global-research.github.io/vrtility/reference/vrt_create_mask.md)
  [`create_omnicloudmask()`](https://permian-global-research.github.io/vrtility/reference/vrt_create_mask.md)
  : Create a new mask for a vrt object This function allows you to
  create a mask for a VRT object based on specified input bands and a
  mask function.

## VRT helpers

Helper functions for VRT objects

- [`vrt_set_band_names()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_band_names.md)
  : set the band names for a vrt_x object
- [`vrt_save()`](https://permian-global-research.github.io/vrtility/reference/vrt_save.md)
  : Save a vrt_block object to disk
- [`vrt_add_empty_band()`](https://permian-global-research.github.io/vrtility/reference/vrt_add_empty_band.md)
  : Add an empty band to a VRT_x object
- [`vrt_move_band()`](https://permian-global-research.github.io/vrtility/reference/vrt_move_band.md)
  : Add an empty band to a VRT_x object
- [`vrt_set_scale()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_scale.md)
  : set the scale values for a VRT_x object
- [`vrt_set_nodata()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_nodata.md)
  : Set NoData Value for VRT
- [`vrt_xml_schema`](https://permian-global-research.github.io/vrtility/reference/vrt_tools.md)
  [`vrt_schema()`](https://permian-global-research.github.io/vrtility/reference/vrt_tools.md)
  : The official GDAL VRT XML schema A copy of the official VRT schema
  for vritility VRT validation.

## multi-band reduction funcions

Functions to create composites using multi-band algorithms

- [`multiband_reduce()`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md)
  [`geomedian()`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md)
  [`medoid()`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md)
  [`quantoid()`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md)
  [`geomedoid()`](https://permian-global-research.github.io/vrtility/reference/multiband_reduce.md)
  : Image composite reductions that require all bands.

## single-band many-to-many functions

Functions designed to work with time-series, namely applying outlier
filtering.

- [`singleband_m2m()`](https://permian-global-research.github.io/vrtility/reference/singleband-many-to-many.md)
  [`hampel_filter()`](https://permian-global-research.github.io/vrtility/reference/singleband-many-to-many.md)
  : Image processing along a time series.

## GDAL Configuration

GDAL configuration and setup functions

- [`gdal_config_opts()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`gdal_creation_options()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`gdalwarp_options()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`set_gdal_config()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`set_gdal_cache_max()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`gdal_raster_drivers()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  [`check_muparser()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md)
  : Create and set GDAL configuration options.

## Python Integration

Python environment and computation environment

- [`vrtility_py_require()`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md)
  [`set_py_env_vals()`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md)
  [`compute_with_py_env()`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md)
  : Setup the vrtility Python environment

## STAC Functions

STAC query and planetary computer functions

- [`stac_query()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`sentinel2_stac_query()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`hls_stac_query()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`sentinel1_stac_query()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`stac_cloud_filter()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`stac_orbit_filter()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`stac_coverage_filter()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`sign_mpc_items()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  [`stac_drop_duplicates()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md)
  : Query a STAC source

## Raster visualisation

plotting functions for raster files and vrt_x objects

- [`plot(`*`<Rcpp_GDALRaster>`*`)`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  [`plot_raster_src()`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  [`plot(`*`<vrt_block>`*`)`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  [`plot(`*`<vrt_stack>`*`)`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  [`plot(`*`<vrt_stack_warped>`*`)`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  [`plot(`*`<vrt_collection>`*`)`](https://permian-global-research.github.io/vrtility/reference/plot_raster.md)
  :

  plot a raster file or `vrt_x` object

## Cache management

Helper functions for managing / altering vrtility cache settings

- [`vrt_cache_set()`](https://permian-global-research.github.io/vrtility/reference/vrt_cache_management.md)
  [`vrt_cache_destroy()`](https://permian-global-research.github.io/vrtility/reference/vrt_cache_management.md)
  : Set the VRT cache directory

## parallel processing helpers

Helper functions for parallel processing

- [`n_daemons()`](https://permian-global-research.github.io/vrtility/reference/mirai-mgmt.md)
  [`daemons_load_vrtility()`](https://permian-global-research.github.io/vrtility/reference/mirai-mgmt.md)
  : Mirai Daemon Management

## Spatial helper functions

Helper functions for spatial operations

- [`bbox_to_projected()`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md)
  [`ogr_bbox_from_file()`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md)
  [`ogr_srs_from_file()`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md)
  : Project a lat/long bounding box to a generic projected coordinate
  system

## gdalraster helpers

Helper functions for GDALRasterDataset objects

- [`r_to_MEM()`](https://permian-global-research.github.io/vrtility/reference/gdalraster-helpers.md)
  : Create an in-memory GDAL raster dataset from R data
