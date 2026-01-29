# Construct A warped VRT or warped VRT collection.

Construct A warped VRT or warped VRT collection.

## Usage

``` r
vrt_warp(
  x,
  t_srs,
  te,
  tr,
  resampling = c("bilinear", "near", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(COMPRESS = "NONE", PREDICTOR = NULL),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
)

# S3 method for class 'vrt_block'
vrt_warp(
  x,
  t_srs,
  te,
  tr,
  resampling = c("bilinear", "near", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(COMPRESS = "NONE", PREDICTOR = NULL),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
)

# S3 method for class 'vrt_collection'
vrt_warp(
  x,
  t_srs,
  te,
  tr,
  resampling = c("bilinear", "near", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  quiet = TRUE,
  lazy = NULL,
  creation_options = gdal_creation_options(COMPRESS = "NONE", PREDICTOR = NULL),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
)

# S3 method for class 'vrt_plan'
vrt_warp(
  x,
  t_srs,
  te,
  tr,
  resampling = c("bilinear", "near", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  quiet = TRUE,
  lazy = FALSE,
  creation_options = gdal_creation_options(COMPRESS = "NONE", PREDICTOR = NULL),
  warp_options = gdalwarp_options(),
  config_options = gdal_config_options()
)
```

## Arguments

- x:

  A vrt_collection or vrt_block (most likely the former).

- t_srs:

  character target SRS must be a numeric EPSG code, or SRS like
  character such as a proj4 string or WKT.

- te:

  numeric vector of the target extent in the form c(xmin, ymin, xmax,
  ymax) using the same SRS as in `t_srs`.

- tr:

  numeric vector of the target resolution in the form c(xres, yres)

- resampling:

  character vector of the resampling methods to be used for each band.
  The default is "bilinear". "near" sampling will be used for the
  mask_band if provided.

- quiet:

  logical indicating whether to suppress progress bar.

- lazy:

  logical indicating whether to create virtual warped files (TRUE) or to
  materialize the warped files to disk (FALSE). When working with remote
  data sources, lazy=FALSE is strongly recommended to improve
  performance. When NULL (default) the function will decide based on
  whether the input data is remote or local.

- creation_options:

  A character vector of options to pass to the the gdal "engine".

- warp_options:

  A character vector of options to pass to gdalwarp

- config_options:

  A character vector of options to set in the GDAL environment

## Details

This function generates warped VRT objects types. This is particularly
useful when we want to create a vrt_stack but our input images span
multiple spatial reference systems. In such a situation, before warping
our input data we must align with our desired output grid.

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
ex_collect <- vrt_collect(s2files)
t_block <- ex_collect[[1]][[1]]
vrt_warp(
ex_collect,
   t_srs = t_block$srs,
   te = t_block$bbox,
   tr = t_block$res
 )
#> → <VRT Collection>
#> 
#>  VRT SRS: 
#> PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
#> 
#> 
#> Bounding Box: 289813.58 88876.43 297031.04 95714.02
#> Pixel res: 19.9929669011164, 19.9929669011164
#> Start Date: NA
#> End Date: NA
#> Number of Items: 5
#> Assets: B02, B03, B04, B08, SCL
```
