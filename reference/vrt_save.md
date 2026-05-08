# Save a vrt_block object to disk

Save a vrt_block object to disk

## Usage

``` r
vrt_save(x, outfile, bundle = FALSE, include_rasters = FALSE)
```

## Arguments

- x:

  A `vrt_stack` of `vrt_block` object.

- outfile:

  A character string of the output file

- bundle:

  Logical. If `TRUE`, copy every intermediate VRT in the dependency tree
  into the directory containing `outfile` and rewrite all
  `<SourceFilename>` paths relative to that directory. The result is a
  self-contained bundle that survives wiping the VRT cache. Remote
  sources (URLs, `/vsicurl/`, ...) cannot be bundled and are left
  untouched.

- include_rasters:

  Logical. Only meaningful when `bundle = TRUE`. If `TRUE`, copy local
  raster leaves (e.g. GeoTIFFs) into the bundle as well, producing a
  fully portable directory. Remote raster sources are skipped and
  surfaced as a single warning.
