# Construct the base VRT object for composing VRT pipelines.

Construct the base VRT object for composing VRT pipelines.

## Usage

``` r
vrt_collect(x, ...)

# S3 method for class 'character'
vrt_collect(
  x,
  config_opts = gdal_config_options(),
  bands = NULL,
  band_descriptions = NULL,
  datetimes = rep("", length(x)),
  vsi_prefix = "",
  driver = "",
  check_src = TRUE,
  ...
)

# S3 method for class 'doc_items'
vrt_collect(
  x,
  config_opts = gdal_config_options(),
  vsi_prefix = "",
  driver = "",
  ...
)

# S3 method for class 'vrt_collection'
print(x, xml = FALSE, pixfun = FALSE, maskfun = FALSE, blocks = FALSE, ...)

# S3 method for class 'vrt_block'
c(x, ...)

# S3 method for class 'vrt_collection'
c(x, ...)
```

## Arguments

- x:

  An object to be used to create a vrt_x object see details.

- ...:

  In the case of `c`, additional vrt_collection objects to concatenate
  to `x`. Otherwise, additional arguments to pass to the method or
  unused.

- config_opts:

  A named character vector of GDAL configuration options.

- bands:

  A numeric vector of band indices to include in the VRT collection

- band_descriptions:

  A character vector of band descriptions.

- datetimes:

  A character vector of datetimes.

- vsi_prefix:

  A character string indicating the VSI prefix to use for the VRT
  sources. Defaults to `"/vsicurl/"`. See
  [`vsi_get_fs_prefixes`](https://usdaforestservice.github.io/gdalraster/reference/vsi_get_fs_prefixes.html)
  for available options.

- driver:

  A character string indicating the GDAL driver to use for the input
  source(s). if "" is provided, the driver will be automatically
  determined by GDAL. for available drivers use
  [`gdal_raster_drivers`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md).

- check_src:

  A logical indicating whether to check that the source files exist.
  Default is TRUE.

- xml:

  logical indicating whether to print the XML of the VRT collection.

- pixfun:

  logical indicating whether to print the pixel function.

- maskfun:

  logical indicating whether to print the mask function.

- blocks:

  A logical indicating whether to print the blocks instead of the
  collection summary.

## Value

A vrt_collection object.

## Details

The main way to create a vrt_collection object, which forms the basis of
the vrrt-based pipelines in vrtility is using a doc_items object from
the `rstac` package. For more info on how to create a doc_items object
see
[`stac_query()`](https://permian-global-research.github.io/vrtility/reference/stac_utilities.md).
To build a vrt_stack object a vrt_collection is required first. The
vrt_collection object is essentially a list of VRT files. At this stage
no alignment is carried out - and the rasters are virtualised as-is. In
this state, we can apply masks, for example and when summarisation is
required we can use vrt_stack - however, in order to create a stack the
collection must contain images from a single spatial reference system
(SRS). If there are mutliple SRS values, use
[`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
to unify the projection of the collection (This is almost always a good
idea anyway).

We can also create a VRT collection from a set of files. This is useful
when we have the data on disk or as a downstream step after first
processing a stac collection.

You can use the `c` method to combine multiple vrt_collection objects.
All collections must have the same number of bands.

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
vrt_collect(s2files)
#> → <VRT Collection>
#> 
#>  VRT SRS: 
#> PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
#> 
#>  PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
#> 
#>  PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
#> 
#> 
#> Bounding Box: NA
#> Pixel res: 19.9923198138287, 19.9923198138287
#> Start Date: NA
#> End Date: NA
#> Number of Items: 5
#> Assets: B02, B03, B04, B08, SCL

# we can also combine multiple vrt collections
c(vrt_collect(s2files[1:2]), vrt_collect(s2files[3:4]))
#> → <VRT Collection>
#> 
#>  VRT SRS: 
#> PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
#> 
#>  PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
#> 
#>  PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
#> 
#> 
#> Bounding Box: NA
#> Pixel res: 19.9923198138287, 19.9923198138287
#> Start Date: NA
#> End Date: NA
#> Number of Items: 4
#> Assets: B02, B03, B04, B08, SCL

if (FALSE) { # interactive()
s2q <- sentinel2_stac_query(
 bbox = c(-12.386, -37.214, -12.186, -37.014),
 start_date = "2023-01-01",
 end_date = "2023-01-31",
 max_cloud_cover = 10,
 assets = c("B02", "B03", "B04", "B08", "SCL")
)

vrt_collect(s2q)
}
```
