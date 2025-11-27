# Stack VRT files from a vrt_collection object

Stack VRT files from a vrt_collection object

Print a vrt_block object

## Usage

``` r
vrt_stack(x)

# S3 method for class 'vrt_collection'
vrt_stack(x)

# S3 method for class 'vrt_stack'
print(x, xml = FALSE, pixfun = FALSE, maskfun = FALSE, ...)
```

## Arguments

- x:

  A vrt_collection object

- xml:

  A logical indicating whether to print the XML

- pixfun:

  A logical indicating whether to print the pixel function

- maskfun:

  A logical indicating whether to print the mask function

- ...:

  Additional arguments not used

## Value

A vrt_stack object

## Details

This function stacks VRT files from a vrt_collection object into a
single VRT file containing multiple layers for each RasterBand. The VRT
files are stacked in the order they are provided in the vrt_collection
object. If this is derived from a rstac object, the order should be
ordered by date.
