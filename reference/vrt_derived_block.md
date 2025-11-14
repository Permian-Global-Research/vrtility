# Create vrt blocks with derived bands.

Create new vrt blocks containing derived expression bands.

## Usage

``` r
vrt_derived_block(x, ...)
```

## Arguments

- x:

  a `vrt_block` or `vrt_collection` object.

- ...:

  named R formulas for new bands to create. Formula variable names must
  the names of existing bands in the vrt_block object. the argument name
  is used to name the new (derived) band.

## Examples

``` r
if (FALSE) { # check_muparser()
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

ex_collect <- vrt_collect(s2files[1])

ex_ndvi <- vrt_derived_block(
  ex_collect,
  ndvi ~ (B08 - B04) / (B08 + B04)
)
plot(ex_ndvi)
}
```
