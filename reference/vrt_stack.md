# Stack VRT files from a vrt_collection object

Stack VRT files from a vrt_collection object

Print a vrt_block object

## Usage

``` r
vrt_stack(x, ...)

# S3 method for class 'vrt_collection'
vrt_stack(
  x,
  quiet = TRUE,
  lazy = TRUE,
  engine = "warp",
  creation_options = gdal_creation_options(COMPRESS = "NONE", PREDICTOR = NULL,
    NUM_THREADS = 1, TILED = "NO"),
  warp_options = gdalwarp_options(num_threads = 1),
  config_options = gdal_config_opts(GDAL_NUM_THREADS = 1, GDAL_HTTP_MULTIPLEX = "NO"),
  ...
)

# S3 method for class 'vrt_stack'
print(x, xml = FALSE, pixfun = FALSE, maskfun = FALSE, ...)
```

## Arguments

- x:

  A vrt_collection object

- ...:

  Additional arguments not used

- quiet:

  Logical. If TRUE, suppress GDAL progress bar

- lazy:

  If TRUE, the incoming blocks will remain virtual. if FALSE,
  vrt_compute is called and the data is materialised to disk. using
  FALSE is significantly faster for remote data sources.

- engine:

  A character vector of the engine to use for processing the raster
  data. See details.

- creation_options:

  A character vector of options to pass to the the gdal "engine".

- warp_options:

  A character vector of options to pass to gdalwarp

- config_options:

  A character vector of options to set in the GDAL environment

- xml:

  A logical indicating whether to print the XML

- pixfun:

  A logical indicating whether to print the pixel function

- maskfun:

  A logical indicating whether to print the mask function

## Value

A vrt_stack object

## Details

This function stacks VRT files from a vrt_collection object into a
single VRT file containing multiple layers for each RasterBand. The VRT
files are stacked in the order they are provided in the vrt_collection
object. If this is derived from a rstac object, the order should be
ordered by date.
