
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vrtility

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of vrtility is to make the best use of GDAL’s VRT capabilities
for efficient processing of large raster datasets. This package’s
primary focus is on the use of GDAL VRT pixel functions using
[numba](https://numba.pydata.org/). for very efficient raster
processing. At present the only function provided is a simple median.
There is definitely scope to expand this functionality! Comments and
contributions are most welcome!

## Features

- No intermediate downloads - the use of nested VRTs enables the
  download and processing of only the required data in a single gdalwarp
  call.

- use of numba in python pixel function(s)

## TO DO:

- [ ] Add efficient masking.
- [ ] Add more pixel functions (geometric median in particular).
- [ ] clean things up a lot!
- [ ] time series functions…
- [ ] configure GPU processing with numba?

…

## Installation

You can install the development version of vrtility from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("Permian-Global-Research/vrtility")
# next set up the required Python environment
vrtility::build_vrtility_python()
```

### Possible extra steps.

For now, {vrtility} requires the use of a virtual python environment -
this is a lighter and safer way to manage python envs than with conda or
the system python, respectively. It may be necessary to install the
virtualenv package for python3.

This can be done on Debian/Ubuntu with:

    sudo apt-get install python3-venv

or if on MacOS or Windows, you can install it with pip:

    python3 -m pip install virtualenv

## Example

Here is a simple example where we: define a bounding box, search a STAC
catalog for Sentinel-2 data, create a VRT, and then create a composite
image using a median pixel function.

``` r
library(vrtility)
library(tictoc)

bbox <- gdalraster::bbox_from_wkt(
  wkt = wk::wkt("POINT (130.75 -11.45)"),
  extend_x = 0.17, extend_y = 0.125
)

tic()
s2_vrt <- sentinel2_stac_vrt(
  bbox = bbox,
  start_date = "2023-01-01",
  end_date = "2023-05-30",
  assets = c(
    # "B01",
    "B02", "B03", "B04"
    # "B05", "B06", "B07",
    # "B08", "B8A", "B09",
    # "B11", "B12"
  )
)
print(s2_vrt)
#> → STAC VRT
#> VRT XML: [hidden]
#>   run print(x, xml = TRUE) to view
#> Bounding Box: 130.58 -11.575 130.92 -11.325
#> Start Date: 2023-01-01
#> End Date: 2023-05-30
#> Number of Items: 9
#> Assets: B02, B03, B04
```

``` r

s2_vrt_pf <- vrt_add_pixfun(s2_vrt) # add a median pixel function to vrt

print(s2_vrt_pf)
#> → STAC VRT
#> VRT XML: [hidden]
#>   run print(x, xml = TRUE) to view
#> Pixel Function: [hidden]
#>   run print(x, pixfun = TRUE) to view
#> Bounding Box: 130.58 -11.575 130.92 -11.325
#> Start Date: 2023-01-01
#> End Date: 2023-05-30
#> Number of Items: 9
#> Assets: B02, B03, B04
```

``` r

s2_composite <- vrt_composite(
  s2_vrt_pf,
  outfile = fs::file_temp(ext = ".tif"),
  bbox = bbox
)
#> 0...10...20...30...40...50...60...70...80...90...100 - done.
```

``` r
toc()
#> 57.579 sec elapsed
```

``` r

terra::plotRGB(
  terra::rast(s2_composite), 3, 2, 1,
  stretch = "lin", zlim = c(500, 2200), zcol = TRUE,
  mar = c(2, 2, 2, 2),
  axes = TRUE
)
```

<img src="man/figures/README-example-1.png" width="100%" />
