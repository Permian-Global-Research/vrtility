
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

Here is a simple example where we: define a bounding box, search a STAC
catalog for Sentinel-2 data, create a vrt_collection object (basically a
list of warped VRTs). We then apply the masking using pixel functions (I
can’t get internal or external masks to work for these VRTs), finally
these images are stacked (combined into a single VRT with multiple
layers in each VRTRasterBand), the median pixel function is then added
to the VRT and all of this is then “lazily” calculated at the end of the
vrt pipeline using gdalwarp.

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
  vrt_set_maskfun(mask_band = "SCL", valid_bits = c(4, 5, 6, 7, 11)) |>
  vrt_warp(t_srs = trs, te = te, tr = c(10, 10)) |>
  vrt_stack() |>
  vrt_set_pixelfun() |>
  vrt_compute(
    outfile = fs::file_temp(ext = "tif"),
    warp_options = gdalwarp_options(num_threads = "ALL_CPUS")
  )
toc()
#> 99.694 sec elapsed
```

``` r

plot_raster_src(
  median_composite,
  c(3, 2, 1)
)
```

<img src="man/figures/README-example-1.png" width="100%" />
