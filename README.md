
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vrtility

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Permian-Global-Research/vrtility/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Permian-Global-Research/vrtility/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Permian-Global-Research/vrtility/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Permian-Global-Research/vrtility?branch=main)
<!-- badges: end -->

The goal of vrtility is to make the best use of GDAL’s VRT capabilities
for efficient processing of large raster datasets - mainly with Earth
Observation in mind. This package’s primary focus is on the use of GDAL
VRT pixel functions using python. These python pixel functions are used
to apply cloud masks and summarise pixel values (e.g. median) from
multiple images (i.e create a composite image). For now we’re just using
the python pixel function capabilities but hope to add C++ or
expressions in time.

## Features

- No intermediate downloads - the use of nested VRTs enables the
  download and processing of only the required data in a single gdalwarp
  (or gdal_translate) call. This reduces disk read/write time.

- use of numba in python pixel function(s) - not always faster but can
  be.

- modular design: We’re basically creating remote sensing pipelines
  using nested VRTs. This allows for the easy addition of new pixel
  functions and masking functions. but could easily be adapted for
  deriving spectral indices or calculating complex time series
  functions.

- parallel processing of VRTs using `dask` and `rioxarray`

## TO DO:

- [ ] Add additional pixel functions (geometric median in particular).
- [ ] time series functions…

## Installation

You can install the development version of vrtility from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("Permian-Global-Research/vrtility")
```

## Example

Here is a simple example where we:

1.  Define a bounding box and search a STAC catalog for Sentinel-2 data

2.  Create a `vrt_collection` object - essentially a list of individual
    VRTs (each making up one image) which we refer to as `vrt_block`s in
    this package.

3.  Then, we apply the mask using pixel functions. This simply modifies
    the XML of the VRT “blocks”.

4.  Because this set of images have more than one common spatial
    reference system (SRS) we convert the `vrt_block`s in the
    `vrt_collection` to warped VRTs, giving us a `vrt_collection_warped`
    object.

5.  These images are then “stacked” (combined into a single VRT with
    multiple layers in each VRTRasterBand), giving us a `vrt_stack`
    object.

6.  A median pixel function is then added to the `vrt_stack`.

7.  all of this is then “lazily” computed at the end of the vrt pipeline
    python’s `rioxarry` package along with `dask` for parallel
    processing. To make use of the rioxarray “engine”, we have to use
    vrt_warp on our `vrt_collection` first.

``` r
library(vrtility)
library(tictoc)

bbox <- gdalraster::bbox_from_wkt(
  wkt = wk::wkt("POINT (144.3 -7.6)"),
  extend_x = 0.17,
  extend_y = 0.125
)

te <- bbox_to_projected(bbox)
trs <- attr(te, "wkt")

s2_stac <- sentinel2_stac_query(
  bbox = bbox,
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  max_cloud_cover = 35,
  assets = c("B02", "B03", "B04", "SCL")
)
# number of items:
length(s2_stac$features)
#> [1] 12
```

``` r

tic()
median_composite <- vrt_collect(s2_stac) |>
  vrt_set_maskfun(
    mask_band = "SCL",
    mask_values = c(0, 1, 2, 3, 8, 9, 10, 11)
  ) |>
  vrt_warp(t_srs = trs, te = te, tr = c(10, 10)) |>
  vrt_stack() |>
  vrt_set_pixelfun() |>
  vrt_compute(
    outfile = fs::file_temp(ext = "tif"),
    engine = "rioxarray"
  )
#> ℹ Dask dashboard @: http://127.0.0.1:8787/status
```

``` r
toc()
#> 77.903 sec elapsed
```

``` r

plot_raster_src(
  median_composite,
  c(3, 2, 1)
)
```

<img src="man/figures/README-example-1.png" width="100%" />

We can also use on-disk raster files too, as shown here with this
example dataset - note that the inputs have multiple spatial reference
systems and therefore we need to warp them (as in the above example)
before stacking. If your images are all in the same CRS, you will save a
lot of time by warping only once in `vrt_compute`. We can plot thse
`vrt_{x}` objects using the `plot` but note that for very large rasters
where we are computing pixel functions this can be slow and we are
better off using `vrt_compute` to write to disk and then plotting the
output.

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))[1:4]

ex_collect <- vrt_collect(s2files)
par(mfrow = c(2, 2))
purrr::walk(
  seq_len(ex_collect$n_items),
  ~ plot(ex_collect, item = .x, bands = c(3, 2, 1))
)
```

<img src="man/figures/README-example2-1.png" width="100%" />

``` r

ex_collect_mask <- vrt_set_maskfun(
  ex_collect,
  mask_band = "SCL",
  mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
)

purrr::walk(
  seq_len(ex_collect_mask$n_items),
  ~ plot(ex_collect_mask, item = .x, bands = c(3, 2, 1))
)
```

<img src="man/figures/README-example2-2.png" width="100%" />

``` r

# extract a block to use as a template for warping
t_block <- ex_collect[[1]][[4]]

ex_composite <- vrt_warp(
  ex_collect_mask,
  t_srs = t_block$srs,
  te = t_block$bbox,
  tr = c(20, 20)
) |>
  vrt_stack() |>
  vrt_set_pixelfun(pixfun = median_numpy())

par(mfrow = c(1, 1))
plot(ex_composite, bands = c(3, 2, 1), quiet = TRUE)
```

<img src="man/figures/README-example2-3.png" width="100%" />

``` r

# write to disk if we wanted to...
# vrt_compute(
#   ex_composite,
#   outfile = fs::file_temp(ext = "tif"),
#   engine = "warp"
# )
```
