# Create a VRT execution plan from STAC items

`vrt_plan()` creates a lightweight metadata structure from rstac items
without creating VRT files. This plan can be passed directly to
[`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
for efficient processing, skipping the intermediate VRT creation step
that
[`vrt_collect()`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
performs.

## Usage

``` r
vrt_plan(x, vsi_prefix = "", driver = "", ...)

# S3 method for class 'doc_items'
vrt_plan(x, vsi_prefix = "", driver = "", ...)
```

## Arguments

- x:

  A `doc_items` object from rstac.

- vsi_prefix:

  Character string specifying the GDAL virtual file system prefix (e.g.,
  "/vsicurl/", "/vsis3/"). Default is empty string which lets rstac
  determine the appropriate prefix.

- driver:

  Character string specifying a driver prefix to prepend to source
  paths.

- ...:

  Additional arguments (unused).

## Value

A `vrt_plan` object containing:

- sources:

  List of items, each containing source URLs per asset

- assets:

  Character vector of asset/band names

- date_time:

  Character vector of datetimes (one per item)

- n_items:

  Number of items in the plan

- vsi_prefix:

  VSI prefix used

- driver:

  Driver prefix used

- config_options:

  GDAL configuration options

## Details

Unlike
[`vrt_collect()`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md),
`vrt_plan()` does not create any VRT files or make GDAL calls to read
spatial metadata. This makes it faster when you intend to immediately
warp the data to a common grid.

The trade-off is that spatial metadata (CRS, bounding box, resolution)
is not available until warping occurs. Use
[`vrt_collect()`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
if you need to inspect or use the source spatial properties before
warping.

## See also

[`vrt_collect()`](https://permian-global-research.github.io/vrtility/reference/vrt_collect.md)
for full VRT creation,
[`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
for warping

## Examples

``` r
if (FALSE) { # \dontrun{
# Query STAC items
items <- sentinel2_stac_query(
  bbox = c(-12.5, -37.5, -12.0, -37.0),
  start_date = "2023-01-01",
  end_date = "2023-01-31",
  max_cloud_cover = 10,
  assets = c("B02", "B03", "B04", "SCL")
)

# Create plan (fast, no VRT creation)
plan <- vrt_plan(items)

# Warp directly from plan
warped <- vrt_warp(
  plan,
  t_srs = "EPSG:32724",
  te = c(700000, 5850000, 710000, 5860000),
  tr = c(10, 10)
)
} # }
```
