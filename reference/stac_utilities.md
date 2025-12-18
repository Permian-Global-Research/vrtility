# Query a STAC source

A set of helper functions to query STAC APIs. These are very thin
wrappers around existing rstac functions, but provide some convenience
for common tasks.

## Usage

``` r
stac_query(
  bbox,
  stac_source,
  collection,
  start_date,
  end_date,
  assets = NULL,
  mpc_sign = grepl("planetarycomputer\\.microsoft\\.com", stac_source),
  drop_duplicates = TRUE,
  check_collection = TRUE
)

sentinel2_stac_query(
  bbox,
  start_date,
  end_date,
  assets = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11",
    "B12", "SCL"),
  max_cloud_cover = 10,
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = "sentinel-2-l2a"
)

hls_stac_query(
  bbox,
  start_date,
  end_date,
  assets = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10",
    "B11", "B12", "Fmask"),
  max_cloud_cover = 10,
  stac_source = c("https://planetarycomputer.microsoft.com/api/stac/v1/",
    "https://cmr.earthdata.nasa.gov/stac/LPCLOUD/"),
  collection = c("hls2-s30", "hls2-l30", "HLSS30_2.0", "HLSL30_2.0")
)

sentinel1_stac_query(
  bbox,
  start_date,
  end_date,
  assets = c("hh", "hv", "vh", "vv"),
  orbit_state = c("descending", "ascending"),
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = c("sentinel-1-rtc", "sentinel-1-grd")
)

stac_cloud_filter(items, max_cloud_cover)

stac_orbit_filter(items, orbit_state = c("descending", "ascending"))

stac_coverage_filter(items, bbox, min_coverage = 0.5)

sign_mpc_items(items, subscription_key = Sys.getenv("MPC_TOKEN", unset = NA))

stac_drop_duplicates(items)
```

## Arguments

- bbox:

  A numeric vector of length 4 representing a bounding box

- stac_source:

  The STAC source to query

- collection:

  The collection to query

- start_date:

  The start date for the query

- end_date:

  The end date for the query

- assets:

  A character vector of the asset names to include

- mpc_sign:

  A logical indicating whether to sign the items using the Planetary
  Computer API's signing method (only required if using the Planetary
  Computer STAC API).

- drop_duplicates:

  A logical indicating whether to drop duplicate items from the
  collection. Duplicates are identified based on identical bounding
  boxes (with a precision of 4 decimal places) and datetime properties.
  Duplicate items in the Microsoft Planetary Computer STAC API are a
  known issue.

- check_collection:

  A logical, should the collection name be checked against available
  collections in the `stac_source`? If FALSE, the collection is assumed
  to exist.

- max_cloud_cover:

  A numeric value of the maximum cloud cover percentage

- orbit_state:

  A character string of the orbit state to filter by

- items:

  A rstac doc_items object

- min_coverage:

  A numeric value between 0 and 1 representing the minimum coverage of
  the bounding box required to retain the item

- subscription_key:

  A subscription key associated with your MPC account. This key will be
  automatically used if the environment variable `MPC_TOKEN` is set.

## Value

A rstac doc_items object

A STACItemCollection object with signed assets url.

A rstac doc_items object with duplicate items removed

## Details

Using these functions to produce a `doc_items` object is optional; if
the user prefers the greater flexibility of using rstac directly, they
are encouraged to do so. These functions are only intended to provide a
convenient way to interact with STAC APIs.

The Microsoft Planetary Computer (MPC) STAC API is a very convenient,
large and open catalogue of EO data. To access these assets, urls must
be signed - this can be done using `sign_mpc_items`,
[`items_sign_planetary_computer`](https://brazil-data-cube.github.io/rstac/reference/items_sign_planetary_computer.html)`, or with GDAL by setting the environment variable `VSICURL_PC_URL_SIGNING`to`YES\`.
see the examples for more details.

`sentinel2_stac_query` facilitates the querying of the Sentinel-2 level
2A data from the Microsoft Planetary Computer STAC API (or other S2 STAC
sources) catalogue to generate a Sentinel 2 rstac doc_items object. This
is a convenience function that saves you needing to remember the exact
stac source and name but is simply a thin wrapper around `stac_query`
and `stac_cloud_filter`. Checks are not performed on the stac source or
collection name; assets are checked following the collection query and
will fail if requested assets are not present in the collection. Note
that some STAC sources use different names for the Sentinel-2 level 2A
assets and the provided defaults may not work for all sources.

The `hls_stac_query` function generates a Harmonized Landsat Sentinel
(HLS) stac collection doc_items object. In order to access HLS data you
will need a NASA Earthdata account. You can create one at
<https://urs.earthdata.nasa.gov/users/new>. Once you have an account,
you can set your credentials using the `earthdatalogin` package as shown
in the examples. You can alternatively use the Microsoft Planetary
Computer STAC API to access HLS data, which does not require an
Earthdata account. Note that you must specify the collection as
`hls2-s30` or `hls2-l30` when using the Planetary Computer STAC API and
that the data is only available from 2020 onwards.

The `sentinel1_stac_query` function generates a Sentinel 1 stac
collection doc_items object. It allows you to query Sentinel 1 data from
the Planetary Computer STAC API. The function returns a collection of
Sentinel 1 items that can be used to generate a Sentinel 1 rstac
doc_items object.

The `stac_cloud_filter` function filters a STAC item collection by cloud
cover percentage. It uses the `eo:cloud_cover` property to filter the
items. Items with a cloud cover percentage less than the specified
`max_cloud_cover` are retained.

The `stac_orbit_filter` function filters a STAC item collection by orbit
state. It uses the `sat:orbit_state` property to filter the items. Items
with an orbit state matching the specified `orbit_state` are retained.
It is intended for use with Sentinel-1 data.

The `stac_coverage_filter` function filters a STAC item collection by
minimum coverage of a bounding box. It calculates the area of
intersection between the bounding box and the item's bounding box, and
retains items where the area of intersection is greater than the
specified `min_coverage` of the bounding box area.

Microsoft no longer provide the ability to generate your own
subscription keys. This can be requested depending on the application;
see here for further details:
https://github.com/microsoft/PlanetaryComputer/issues/457. A key is not
required for data access and a more reliable way to ensure access is to
collocate the workload on the Azure West Europe region.

Unlike the
[`rstac::items_sign_planetary_computer`](https://brazil-data-cube.github.io/rstac/reference/items_sign_planetary_computer.html)
function, this function caches collection-level signing tokens both in
memory and on disk for 45 minutes to avoid repeated requests for the
same collection within a short time period. This can avoid 429
(permission denied) errors when signing many items from the same
collection. Memory cache provides faster access for same-session
requests.

The `stac_drop_duplicates` function removes duplicate items from a STAC
item collection. Duplicates are identified based on identical bounding
boxes ( with a precision of 4 decimal places), datetime properties and
platform. Duplicate items in the Microsoft Planetary Computer STAC API
are a known issue.

## Examples

``` r
if (FALSE) { # interactive()
s2_its <- stac_query(
  bbox = c(-12.386, -37.214, -12.186, -37.014),
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = "sentinel-2-l2a",
  start_date = "2023-01-01",
  end_date = "2023-01-31"
)

# For Microsoft Planetary Computer (MPC) assets, sign the the items using the
# MPC signing `sign_mpc_items()`:
# or use GDAL by setting the following environment variable:
# Sys.setenv("VSICURL_PC_URL_SIGNING" = "YES")
}
if (FALSE) { # interactive()
sentinel2_stac_query(
  bbox = c(-12.386, -37.214, -12.186, -37.014),
  start_date = "2023-01-01",
  end_date = "2023-01-31",
  max_cloud_cover = 10,
  assets = c("B02", "B03", "B04", "B08", "SCL")
)
}
if (FALSE) { # interactive()
hls_query <- hls_stac_query(
  c(144.130, -7.725, 144.470, -7.475),
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  assets = c("B04", "B03", "B02", "Fmask"),
  collection = "HLSS30_2.0",
  max_cloud_cover = 35
)
# in order to download these items (or call further vrt_x functions) you
# will first need to set your credentials. The easiest way to do this is with
# the `earthdatalogin` package. First set your EARTHDATA_USER and
# EARTHDATA_PASSWORD environment variables and then run the following command:

earthdatalogin::edl_netrc(
  username = Sys.getenv("EARTHDATA_USER"),
  password = Sys.getenv("EARTHDATA_PASSWORD")
)
}
if (FALSE) { # interactive()
sentinel1_stac_query(
  bbox = c(-12.386, -37.214, -12.186, -37.014),
  start_date = "2023-01-01",
  end_date = "2023-01-31",
  assets = "vv"
)
}
```
