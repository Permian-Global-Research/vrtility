# Set mask band of a VRT collection

Set mask band of a VRT collection

## Usage

``` r
vrt_set_maskfun(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
)

# S3 method for class 'vrt_block'
vrt_set_maskfun(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
)

# S3 method for class 'vrt_collection'
vrt_set_maskfun(
  x,
  mask_band,
  mask_values,
  build_mask_pixfun = NULL,
  buffer_size = 0,
  drop_mask_band = TRUE
)

build_intmask(use_muparser = getOption("vrtility.use_muparser", FALSE))

build_bitmask(use_muparser = getOption("vrtility.use_muparser", FALSE))
```

## Arguments

- x:

  A VRT collection

- mask_band:

  The name of the mask band

- mask_values:

  A numeric vector of integer or bit values to be masked.

- build_mask_pixfun:

  A character string of the Python code or muparser expression to build
  the mask. If `NULL` (default), automatically uses `build_intmask()`
  which will choose muparser if the option `vrtility.use_muparser` is
  `TRUE` with no buffering, otherwise a Python based implementation is
  used. Provided functions include `build_intmask` and `build_bitmask`.
  See details.

- buffer_size:

  A buffer size to apply to the mask (numeric, default: 0). A buffer
  size \> 0 will dilate the mask by the specified number of pixels. This
  can be useful to remove edge effects around clouds. If a buffer size
  \> 0 is specified, the `scipy` python library will automatically be
  installed and Python will be used (muparser cannot do buffering).

- drop_mask_band:

  Logical. If TRUE, the mask band will be removed from the VRT block.

- use_muparser:

  Logical. If `TRUE` and GDAL \>= 3.12, uses muparser

## Value

A VRT block with the mask band set.

A VRT collection with the mask band set.

## Details

The `build_mask_pixfun` function is used to build the mask band. Where
the mask band is a true bitmask and bit-wise operations are required,
the `build_bitmask()` function should be used. For integer-based
masking, where the mask band is provided as a single band with integer
values, the `build_intmask()` function should be used.

By default (when `build_mask_pixfun = NULL`), the function automatically
selects the most efficient implementation:

- GDAL \>= 3.12 with no buffering: Uses muparser expressions (fastest,
  no Python)

- GDAL \< 3.12 or buffering needed: Uses Python/NumPy

`build_intmask` provides an integer mask function that can be used to
mask out pixels based on a band containing true integer/numeric values.
This would be appropriate for the Sentinel 2A SCL band, for example.

`build_bitmask` provides is a simple bit-wise mask function that can be
used to mask out pixels based on a true bit mask. This function should
be used where bitwise operations are required. e.g. for HLS data, the
"Fmask" band requires bitwise operations to extract the mask values.

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

ex_collect <- vrt_collect(s2files)

# Auto-selects muparser or Python based on GDAL version
ex_collect |>
  vrt_set_maskfun(
    mask_band = "SCL",
    mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
    drop_mask_band = FALSE)
#> → <VRT Collection>
#> Mask Function: [hidden]
#>   run print(x, maskfun = TRUE) to view
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

# Force Python implementation
ex_collect |>
  vrt_set_maskfun(
    mask_band = "SCL",
    mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
    build_mask_pixfun = build_intmask(),
    drop_mask_band = FALSE)
#> → <VRT Collection>
#> Mask Function: [hidden]
#>   run print(x, maskfun = TRUE) to view
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
```
