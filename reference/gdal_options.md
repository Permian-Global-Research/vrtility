# Create and set GDAL configuration options.

This function sets GDAL configuration options that can be used to
control the behavior of GDAL operations. These options can be used to
optimize performance, control caching, and manage HTTP requests (among
other things)

## Usage

``` r
gdal_config_options(
  VSI_CACHE = "TRUE",
  VSI_CACHE_SIZE = "268435456",
  GDAL_NUM_THREADS = 1,
  GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
  CPL_VSIL_CURL_CACHE_SIZE = "1342177280",
  GDAL_HTTP_MAX_RETRY = "10",
  GDAL_HTTP_RETRY_DELAY = "0.5",
  GDAL_HTTP_MULTIPLEX = "YES",
  GDAL_HTTP_VERSION = "2",
  GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
  GDAL_HTTP_COOKIEFILE = fs::path(tools::R_user_dir("vrtility", which = "cache"),
    "gdal_cookies.txt"),
  GDAL_HTTP_COOKIEJAR = GDAL_HTTP_COOKIEFILE,
  GDAL_MAX_DATASET_POOL_SIZE = NULL,
  GDAL_INGESTED_BYTES_AT_OPEN = NULL,
  CPL_VSIL_CURL_ALLOWED_EXTENSIONS = NULL,
  CPL_VSIL_CURL_USE_HEAD = "YES",
  CPL_VSIL_CURL_CHUNK_SIZE = NULL,
  ...
)

gdal_creation_options(
  output_format = NULL,
  COMPRESS = "DEFLATE",
  PREDICTOR = "2",
  NUM_THREADS = "ALL_CPUS",
  BIGTIFF = "IF_NEEDED",
  TILED = if (identical(output_format, "COG")) NULL else "YES",
  BLOCKXSIZE = NULL,
  BLOCKYSIZE = NULL,
  COPY_SRC_OVERVIEWS = if (identical(output_format, "COG")) NULL else "YES",
  cli_format = FALSE,
  ...
)

gdalwarp_options(
  multi = FALSE,
  warp_memory = "50%",
  num_threads = if (identical(.Platform$OS.type, "windows")) NULL else 1,
  unified_src_nodata = c("NO", "YES", "PARTIAL")
)

set_gdal_config(x, scope = c("gdalraster", "sys"))

set_gdal_cache_max(mem_fraction = 0.1)

gdal_raster_drivers(shortname = FALSE)

check_muparser(gdal_version = "3.11.4")
```

## Arguments

- VSI_CACHE:

  Should the Virtual File System (VSI) cache be used?

- VSI_CACHE_SIZE:

  Size of the VSI cache in bytes.

- GDAL_NUM_THREADS:

  Number of threads to use for processing. Default is the number of
  available cores divided by the number of daemons.

- GDAL_DISABLE_READDIR_ON_OPEN:

  Disable directory listing on open?

- CPL_VSIL_CURL_CACHE_SIZE:

  Cache size for HTTP requests.

- GDAL_HTTP_MAX_RETRY:

  Maximum number of retries for HTTP requests.

- GDAL_HTTP_RETRY_DELAY:

  Delay between retries in seconds.

- GDAL_HTTP_MULTIPLEX:

  Use HTTP multiplexing?

- GDAL_HTTP_VERSION:

  HTTP version to use.

- GDAL_HTTP_MERGE_CONSECUTIVE_RANGES:

  Merge consecutive ranges in HTTP requests?

- GDAL_HTTP_COOKIEFILE:

  Path to the cookie file for HTTP requests.

- GDAL_HTTP_COOKIEJAR:

  Path to the cookie jar for HTTP requests.

- GDAL_MAX_DATASET_POOL_SIZE:

  Maximum size of the dataset pool.

- GDAL_INGESTED_BYTES_AT_OPEN:

  Number of bytes to read at open.

- CPL_VSIL_CURL_ALLOWED_EXTENSIONS:

  Allowed file extensions for HTTP requests.

- CPL_VSIL_CURL_USE_HEAD:

  Use HTTP HEAD requests?

- CPL_VSIL_CURL_CHUNK_SIZE:

  Chunk size for HTTP requests.

- ...:

  Additional -co options to set

- output_format:

  Output format equivalent to -of on the CLI. see details

- COMPRESS:

  Compression method

- PREDICTOR:

  Prediction method

- NUM_THREADS:

  Number of threads

- BIGTIFF:

  Use BigTIFF

- TILED:

  Use tiling

- BLOCKXSIZE:

  Block size in X

- BLOCKYSIZE:

  Block size in Y

- COPY_SRC_OVERVIEWS:

  Copy source overviews

- cli_format:

  Logical indicating whether to return the options in a format suitable
  for passing to system calls (i.e. with -co prefixes)

- multi:

  Logical indicating whether to use multi-threading, equivalent to
  -multi on the CLI

- warp_memory:

  Memory to use for warping equivalent to -wm on the CLI

- num_threads:

  Number of threads to use for warping equivalent to -wo NUM_THREADS on
  the CLI. "ALL_CPUS" (the default) will use all available CPUs,
  alternatively an integer can be supplied - or NULL to use a single
  threaded process.

- unified_src_nodata:

  Unified source nodata option equivalent to -wo UNIFIED_SRC_NODATA on
  the CLI. Can be "NO", "YES" or "PARTIAL". Default is "NO" (as was the
  default for earlier versions of GDAL).

- x:

  A named character vector of the configuration options

- scope:

  A character vector of the scope to set the options in. Either
  "gdalraster" or "sys".

- mem_fraction:

  Fraction of total RAM to use for GDAL cache, default is 0.1 (10% of
  total RAM)

- shortname:

  Logical indicating whether to return the short names of the drivers
  (default is FALSE)

- gdal_version:

  Minimum GDAL version to check alongside muparser support

## Value

Character vector of options

set_gdal_cache_max returns (invisibly), a
[`memuse`](https://rdrr.io/pkg/memuse/man/constructor.html) object - the
value set for GDAL_CACHEMAX

A character vector of GDAL raster driver shortnames if
`shortname = TRUE`, otherwise a data frame with the full driver
information.

TRUE if muparser is available, FALSE otherwise

## Details

Where a named argument is set to `NULL`, the default GDAL value will be
used. These arguments are currently included as NULL because they could
in theory improve performance but, from our limited testing they either
have no or a negative impact on performance.

output_format, equivalent to `-of` from the gdaltranslate or gdalwarp
CLIs. If NULL, then the output format will be inferred from the file
extension.

set_gdal_cache_max is a very thin wrapper around
[`set_cache_max`](https://firelab.github.io/gdalraster/reference/set_cache_max.html)
that allows you to conveniently set the GDAL_CACHEMAX option as a
fraction of the total RAM on your system.

check_muparser can be used to check if the installed gdal version was
built with muparser support; muparser is required for derived bands
using
[`vrt_derived_block`](https://permian-global-research.github.io/vrtility/reference/vrt_derived_block.md)
or if `options("vrtility.use_muparser")` is set to TRUE when using
[`vrt_set_maskfun`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
with muparser-based masks.

## See also

[GDAL Configuration
Options](https://gdal.org/en/stable/user/configoptions.html)

[GDAL Raster
Drivers](https://gdal.org/en/stable/drivers/raster/index.html#raster-drivers)

[GDAL Warp Options](https://gdal.org/en/stable/programs/gdalwarp.html)

## Examples

``` r
gdal_config_options(GDAL_HTTP_USERPWD = "user:password")
#>                                         VSI_CACHE 
#>                                            "TRUE" 
#>                                    VSI_CACHE_SIZE 
#>                                       "268435456" 
#>                                  GDAL_NUM_THREADS 
#>                                               "1" 
#>                      GDAL_DISABLE_READDIR_ON_OPEN 
#>                                       "EMPTY_DIR" 
#>                          CPL_VSIL_CURL_CACHE_SIZE 
#>                                      "1342177280" 
#>                               GDAL_HTTP_MAX_RETRY 
#>                                              "10" 
#>                             GDAL_HTTP_RETRY_DELAY 
#>                                             "0.5" 
#>                               GDAL_HTTP_MULTIPLEX 
#>                                             "YES" 
#>                                 GDAL_HTTP_VERSION 
#>                                               "2" 
#>                GDAL_HTTP_MERGE_CONSECUTIVE_RANGES 
#>                                             "YES" 
#>                              GDAL_HTTP_COOKIEFILE 
#> "/home/runner/.cache/R/vrtility/gdal_cookies.txt" 
#>                               GDAL_HTTP_COOKIEJAR 
#> "/home/runner/.cache/R/vrtility/gdal_cookies.txt" 
#>                            CPL_VSIL_CURL_USE_HEAD 
#>                                             "YES" 
#>                                 GDAL_HTTP_USERPWD 
#>                                   "user:password" 
gdal_creation_options(COMPRESS = "JPEG", JPEG_QUALITY = "90")
#> [1] "COMPRESS=JPEG"          "PREDICTOR=2"            "NUM_THREADS=ALL_CPUS"  
#> [4] "BIGTIFF=IF_NEEDED"      "TILED=YES"              "COPY_SRC_OVERVIEWS=YES"
#> [7] "JPEG_QUALITY=90"       
gdalwarp_options(multi = TRUE, warp_memory = "50%", num_threads = 4)
#> [1] "-multi"                "-wm"                   "50%"                  
#> [4] "-wo"                   "NUM_THREADS=4"         "-wo"                  
#> [7] "UNIFIED_SRC_NODATA=NO"
set_gdal_config(gdal_config_options())
gcm <- set_gdal_cache_max(0.05)
#> ℹ GDAL_CACHEMAX set to 799.489 MiB; to change this use
#>   `vrtility::set_gdal_cache_max()`.
print(gcm)
#> 799.489 MiB
```
