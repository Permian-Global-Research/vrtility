
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vrtility <a href="https://permian-global-research.github.io/vrtility/" alt="vrtility"><img src="man/figures/vrtility_hex.png" alt="vrtility logo" align="right" width="200"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Permian-Global-Research/vrtility/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Permian-Global-Research/vrtility/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Permian-Global-Research/vrtility/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Permian-Global-Research/vrtility?branch=main)
<!-- badges: end -->

vrtility is an R package that aims to make the best use of
[GDAL](https://gdal.org/en/stable/index.html)’s
[VRT](https://gdal.org/en/stable/drivers/raster/vrt.html) capabilities
for efficient processing of large raster datasets - mainly with Earth
Observation (EO) in mind. vrtility uses the VRT format to access awesome
features such as pixel functions but also harnesses the VRT data
structure to facilitate complex image processing tasks such as
multi-band compositing and time series filtering.

> [!CAUTION]
> This package is under active development and is likely to change. Contributions and suggestions are still very welcome!

## Features

- Modular design: We’re basically creating remote sensing pipelines
  using nested VRTs. This allows for the easy addition of new pixel and
  masking functions. but could easily be adapted for deriving spectral
  indices or calculating complex time series functions. All powered by
  [{gdalraster}](https://usdaforestservice.github.io/gdalraster/index.html).

- vrtility enables the use of GDAL VRT python and built-in pixel
  functions. The python [numpy](https://numpy.org/)-based pixel
  functions can be used to apply cloud masks and summarise pixel values
  (e.g. median) from multiple images (i.e create a composite image). All
  python environment and package management is handled by
  [{reticulate}](https://rstudio.github.io/reticulate/). Built-In GDAL
  pixel functions vary depending on the GDAL version, but are highly
  performant - recent GDAL versions even support
  [expressions](https://mirai.r-lib.org/index.html) for more complex
  operations.

- Efficient parallel processing using
  [{mirai}](https://mirai.r-lib.org/index.html)

- Advanced compositing methods that maintain spectral consistency, such
  as the geometric median and medoid.

- Time series filtering functions to improve temporal consistency and
  reduce noise.

- on-the-fly cloud mask filtering using pixel functions. Ability to use
  [OmniCloudMask](https://github.com/DPIRD-DMA/OmniCloudMask)
  cloud/shadow masking, embedded within the vrt pipeline. (currently
  experimental)

## Installation

You can install vrtility from GitHub with:

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
    reference system (SRS) we warp the `vrt_block`s to a new
    spatially-aligned `vrt_collection` using `vrt_warp`, giving us a
    `vrt_collection_warped` object.

5.  These images are then “stacked” (combined into a single VRT with
    multiple layers in each VRTRasterBand), giving us a `vrt_stack`
    object.

6.  A median pixel function is then added to the `vrt_stack`.

7.  Finally, we calculate the median composite using the `gdalraster`
    engine to write the output which, in combination with the mirai
    package processes the data in parallel across bands and image tiles.

``` r
library(vrtility)

#  Set up asynchronous workers to parallelise vrt_collect and vrt_set_maskfun
mirai::daemons(6)
#> [1] 6
```

``` r

bbox <- gdalraster::bbox_from_wkt(
  wkt = "POINT (144.3 -7.6)",
  extend_x = 0.17,
  extend_y = 0.125
)

te <- bbox_to_projected(bbox)
trs <- attr(te, "wkt")

s2_stac <- sentinel2_stac_query(
  bbox = bbox,
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  max_cloud_cover = 20,
  assets = c("B02", "B03", "B04", "SCL")
)
# number of items:
length(s2_stac$features)
#> [1] 3
```

``` r

system.time({
  median_composite <- vrt_collect(s2_stac) |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11)
    ) |>
    vrt_warp(t_srs = trs, te = te, tr = c(10, 10)) |>
    vrt_stack() |>
    vrt_set_py_pixelfun(pixfun = median_numpy()) |>
    vrt_compute(
      outfile = fs::file_temp(ext = "tif"),
      engine = "gdalraster"
    )
})
#>    user  system elapsed 
#>  15.242   0.514  20.603
```

``` r

plot_raster_src(
  median_composite,
  c(3, 2, 1)
)
```

<img src="man/figures/README-example1-1.png" width="100%" />

## Asynchronous download/processing

{vrtility} uses {mirai}, alongside {purrr} to manage asynchronous
parallelisation. By setting `mirai::daemons(n)` before running the vrt
pipeline, we can sometimes improve performance, depending on the speed
of the server holding the data. In some cases this will make little
difference; for example, the Microsoft Planetary Computer STAC API is
already pretty fast. However, for NASA’s Earthdata STAC API, this can
make a huge difference. In order to use asynchronous processing, in the
`vrt_compute` function, we need to set `engine = "gdalraster"` or we can
use `engine = "warp"` if we are downloading multiple images individually
(This is a much faster approach on Nasa’s Earthdata server).

## Using on-disk rasters

We can also use on-disk raster files (or indeed urls) too, as shown here
with this example dataset - note that the inputs have multiple spatial
reference systems and therefore we need to warp them (as in the above
example) before “stacking”. We can plot these `vrt_{x}` objects using
`plot()` but note that, for very large rasters, where we are computing
pixel functions, this can be slow and we are better off using
`vrt_compute` to write to disk and then plotting the output.

In this example, we create a `medoid` composite from the warped
collection. Using medoid or other multi-band pixel functions
(e.g. `geomedian`) can be extremely powerful but requires more compute
power/time than band-wise pixel functions.

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
  multiband_reduce(reduce_fun = medoid())

par(mfrow = c(1, 1))
```

<img src="man/figures/README-example2-3.png" width="100%" />

``` r
plot_raster_src(ex_composite, bands = c(3, 2, 1))
```

<img src="man/figures/README-example2-4.png" width="100%" />
