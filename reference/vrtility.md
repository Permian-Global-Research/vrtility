# vrtility: GDAL VRT utilities for R

vrtility is a package for building raster (primarily remote sensing)
processing pipelines. It makes use of GDAL's VRT (virtual raster format)
capabilities for efficient processing of large raster datasets. This
package's primary focus is on the use of GDAL VRT pixel functions. These
pixel functions (currently implemented with python) are used to apply
cloud masks and summarise pixel values (e.g. median) from multiple
images (i.e create a composite image). We hope to add C++ or expression
based pixel functions in time.

## Spatial helpers

- [`bbox_to_projected`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Convert a long/lat bounding box to a projected bounding box

- [`ogr_bbox_from_file`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Get a bounding box from a vector file

- [`ogr_srs_from_file`](https://permian-global-research.github.io/vrtility/reference/spatial_helpers.md):

  Get a spatial reference system from a vector file

## STAC helpers

- [`stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog

- [`sentinel2_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for Sentinel-2 data

- [`hls_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for HLS data

- [`stac_cloud_filter`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Filter a STAC collection for cloud cover

- [`sentinel1_stac_query`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Query a STAC catalog for Sentinel-1 data

- [`stac_orbit_filter`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Filter a STAC collection for orbit state

- [`sign_mpc_items`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md):

  Sign STAC items from Microsoft Planetary Computer

## VRT utilities

- [`vrt_collect`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md):

  Create a vrt_collection object

- [`vrt_set_maskfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  Set a mask function for a vrt object

- [`vrt_create_mask`](https://permian-global-research.github.io/vrtility/reference/vrt_create_mask.md):

  Create a mask for a vrt object

- [`vrt_stack`](https://permian-global-research.github.io/vrtility/reference/vrt_stack.md):

  Create a vrt_stack object

- [`vrt_set_py_pixelfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md):

  Set a pixel function for a vrt stack object

- [`vrt_set_gdal_pixelfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_gdal_pixelfun.md):

  Set a GDAL pixel function for a vrt stack object

- [`vrt_set_scale`](https://permian-global-research.github.io/vrtility/reference/vrt_set_scale.md):

  Set the scale and offset values for a vrt_x object

- [`vrt_move_band`](https://permian-global-research.github.io/vrtility/reference/vrt_move_band.md):

  Move a band in a vrt_x object

- [`vrt_set_nodata`](https://permian-global-research.github.io/vrtility/reference/vrt_set_nodata.md):

  Set the nodata value for a vrt_x object

- [`vrt_warp`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md):

  Warp a vrt_x object to a warped vrt

- [`vrt_compute`](https://permian-global-research.github.io/vrtility/reference/vrt_compute.md):

  Compute a vrt pipeline (using GDAL)

## VRT pixel functions

- [`vrt_set_py_pixelfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md):

  Set a pixel function for a vrt stack object

- [`set_mask_numpy`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  A pixel function to apply a bitmask

- [`build_intmask`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  A pixel function to build a mask from a mask band where the mask
  values are integers

- [`build_bitmask`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md):

  A pixel function to build a mask from a mask band where the mask
  values are bit positions

- [`median_numpy`](https://permian-global-research.github.io/vrtility/reference/vrt_set_py_pixelfun.md):

  A pixel function to compute the median

## python environment helpers

- [`vrtility_py_require`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md):

  Require a python package

- [`set_py_env_vals`](https://permian-global-research.github.io/vrtility/reference/vrtility_python.md):

  Set python environment variables as options

## VRT helpers

- [`vrt_save`](https://permian-global-research.github.io/vrtility/reference/vrt_save.md):

  Save a VRT object to a file

- [`vrt_schema`](https://permian-global-research.github.io/vrtility/reference/vrt_tools.md):

  (data object)The official GDAL VRT schema as a character object

## gdalraster helpers

- [`r_to_MEM`](https://permian-global-research.github.io/vrtility/reference/gdalraster-helpers.md):

  Convert a vector to a GDALRasterDataset in memory

## Composite reducers

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

## See also

[LPCLOUD STAC
Browser](https://radiantearth.github.io/stac-browser/#/external/cmr.earthdata.nasa.gov/stac/LPCLOUD/)

## Author

**Maintainer**: Hugh Graham <hugh.graham@permianglobal.com>
([ORCID](https://orcid.org/0000-0001-9451-5010))
