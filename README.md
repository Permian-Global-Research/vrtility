
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vrtility

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of vrtility is to make the best use of GDAL’s VRT capabilities
for efficient processing of large raster datasets. This package is
currently experimental and under active development.

## Features

…

## Installation

You can install the development version of vrtility from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Permian-Global-Research/vrtility")
vrtility::build_vrtility_python()
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(vrtility)
library(tictoc)

bbox <- gdalraster::bbox_from_wkt(
  wkt = wk::wkt("POINT (130.75 -11.45)"),
  extend_x = 0.1, extend_y = 0.125
)

s2_vrt <- sentinel2_stac_vrt(
  bbox = bbox,
  start_date = "2023-01-01",
  end_date = "2023-04-30",
  assets = c(
    # "B01",
    "B02", "B03", "B04"
    # "B05", "B06", "B07",
    # "B08", "B8A", "B09",
    # "B11", "B12"
    # , "SCL"
  )
)

tic()
s2_composite <- vrt_composite(
  s2_vrt,
  outfile = fs::file_temp(ext = ".tif"),
  bbox = bbox
)
#> 0...10...20...30...40...50...60...70...80...90...100 - done.
```

``` r
toc()
#> 19.493 sec elapsed
```

``` r

terra::plotRGB(
  terra::rast(s2_composite), 3, 2, 1,
  stretch = "lin", zlim = c(700, 2200), zcol = TRUE,
  mar = c(2, 2, 2, 2),
  axes = TRUE
)
```

<img src="man/figures/README-example-1.png" width="100%" />
