
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

You may first need to install python and python venv like so:
`apt-get install python3-venv python3-pip python3-dev`

``` r
# install.packages("devtools")
devtools::install_github("Permian-Global-Research/vrtility")
# next set up the required Python environment
vrtility::build_vrtility_python()
```

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
    # , "SCL"
  )
)

print(s2_vrt)
#> $vrt
#> {xml_document}
#> <VRTDataset rasterXSize="20976" rasterYSize="10980">
#> [1] <SRS dataAxisToSRSAxisMapping="1,2">PROJCS["WGS 84 / UTM zone 52S",GEOGCS ...
#> [2] <GeoTransform>  6.0000000000000000e+05,  1.0000000000000000e+01,  0.00000 ...
#> [3] <VRTRasterBand dataType="UInt16" band="1" Description="B02">\n  <NoDataVa ...
#> [4] <VRTRasterBand dataType="UInt16" band="2" Description="B03">\n  <NoDataVa ...
#> [5] <VRTRasterBand dataType="UInt16" band="3" Description="B04">\n  <NoDataVa ...
#> [6] <OverviewList resampling="nearest">2 4 8 16</OverviewList>
#> 
#> $bbox
#> [1] 130.580 -11.575 130.920 -11.325
#> 
#> $start_date
#> [1] "2023-01-01"
#> 
#> $end_date
#> [1] "2023-05-30"
#> 
#> $n_items
#> [1] 9
#> 
#> $assets
#> [1] "B02" "B03" "B04"
#> 
#> attr(,"class")
#> [1] "stac_vrt"
```

``` r

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
#> 54.543 sec elapsed
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
