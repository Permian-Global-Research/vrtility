# Project a lat/long bounding box to a generic projected coordinate system

This function takes a lat/long bounding box and projects it to either a
prescibed projected coordinate system or a generic projected coordinate
system suitable for the AOI.

## Usage

``` r
bbox_to_projected(
  x,
  proj_specific = NULL,
  proj_generic = c("laea", "aeqd", "utm", "pconic", "eqdc"),
  ellps = "WGS84",
  no_defs = TRUE,
  opts = ""
)

ogr_bbox_from_file(x, latlon = FALSE, extend_x = NULL, extend_y = NULL)

ogr_srs_from_file(x)
```

## Arguments

- x:

  character vector. Path to a spatial vector file.

- proj_specific:

  a character vector. The projection to use. A PROJ-readable string or
  an EPSG code. If NULL, a generic projection will be used.

- proj_generic:

  a character vector. The projection to use. One of "laea", "aeqd",
  "utm", "pconic", or "eqdc".

- ellps:

  a character vector. The ellipsoid to use. Select from
  `sf_proj_info(type = "ellps")`.

- no_defs:

  a logical. Whether to include the +no_defs option in the proj string.

- opts:

  a character vector. Additional proj options to pass to the proj
  string. see details for more information.

- latlon:

  logical. Whether to return the bounding box in lat/long (EPSG:4326).
  Default is FALSE.

- extend_x:

  numeric. Amount to extend the bounding box in the x direction (both
  min and max).

- extend_y:

  numeric. Amount to extend the bounding box in the y direction (both
  min and max).

## Value

a numeric vector of length 4 representing the projected bounding box in
the new coordinate system. Attributes include the new proj4 and wkt
string.

A numeric vector of length 4 representing the bounding box ordered as:
xmin, ymin, xmax, ymax.

A character string representing the SRS in WKT format.

## Details

For further info about the available "generic" projects see: for utm:
<https://proj.org/en/9.4/operations/projections/utm.html> for laea:
<https://proj.org/en/9.4/operations/projections/laea.html> for aeqd:
<https://proj.org/en/9.4/operations/projections/aeqd.html> for pconic:
<https://proj.org/en/9.4/operations/projections/pconic.html> for eqdc:
<https://proj.org/en/9.4/operations/projections/eqdc.html>

## Examples

``` r
bbox <- gdalraster::bbox_from_wkt(
  wkt = "POINT (144.3 -7.6)",
  extend_x = 0.17,
  extend_y = 0.125
)

bbox_to_projected(bbox)
#> [1] -18764.54 -13827.92  18764.54  13824.17
#> attr(,"wkt")
#> [1] "PROJCS[\"unknown\",GEOGCS[\"unknown\",DATUM[\"Unknown based on WGS 84 ellipsoid\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]]],PROJECTION[\"Lambert_Azimuthal_Equal_Area\"],PARAMETER[\"latitude_of_center\",-7.6],PARAMETER[\"longitude_of_center\",144.3],PARAMETER[\"false_easting\",0],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"

bbox_to_projected(bbox, proj_generic = "utm")
#> [1]  845317.3 9144543.8  883092.3 9172512.3
#> attr(,"wkt")
#> [1] "PROJCS[\"unknown\",GEOGCS[\"unknown\",DATUM[\"Unknown based on WGS 84 ellipsoid\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",0],PARAMETER[\"central_meridian\",141],PARAMETER[\"scale_factor\",0.9996],PARAMETER[\"false_easting\",500000],PARAMETER[\"false_northing\",10000000],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"


if (FALSE) { # interactive()
bbox_to_projected(
 c(-3.56, 50.69, -3.46, 50.75),
 proj_specific = "EPSG:27700"
)
}
```
