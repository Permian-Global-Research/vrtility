# vrtility 0.4.0.9000

## New features

* `vrt_set_gdal_pixelfun()`, `vrt_set_py_pixelfun()`, `vrt_set_scale()`,
  `vrt_set_nodata()` and `vrt_move_band()` now have `vrt_stack` methods, so
  these operations can be applied after stacking, not only on blocks and
  collections.
* `gdal_config_options()` gained network-robustness controls:
  `GDAL_HTTP_TIMEOUT`, `GDAL_HTTP_CONNECTTIMEOUT`, `GDAL_HTTP_LOW_SPEED_TIME`,
  `GDAL_HTTP_LOW_SPEED_LIMIT`, `GDAL_GEOREF_SOURCES` and `GDAL_PAM_ENABLED`,
  with defaults that stop a stalled `/vsicurl` request from hanging a pipeline.
* `vrt_save()` now defaults `outfile` to a temporary `.vrt` in the VRT cache
  and returns the saved path invisibly.

## Breaking changes

* `vrt_compute()` no longer accepts `apply_scale`; scaling is controlled via
  `vrt_set_scale()` on the input instead.

## Improvements

* `vrt_collect()` opens each source once when building per-item VRTs, removing
  a redundant re-open to read the block size.
* Operations that cannot apply to an already-stacked VRT (`vrt_set_maskfun()`,
  `vrt_create_mask()`, `vrt_add_empty_band()`, and `vrt_save()` on a
  collection) now fail with a clear, actionable error rather than dispatching
  to `default`.

# vrtility 0.4.0

## New features

* `vrt_save()` gained `bundle = TRUE` to write a saved VRT alongside copies
  of every intermediate VRT it depends on, with paths rewritten relative to
  the output directory. The result survives wiping the VRT cache. Adding
  `include_rasters = TRUE` also copies local raster leaves into the bundle
  for full portability; remote sources (URLs, `/vsicurl/`, ...) are detected
  and reported in a single warning (#110).

# vrtility 0.3.3

## Improvements

* Removed usage of `:::` on gdalraster internals; plotting helpers are now
  bundled directly in the package.
* Vignette HTML output now uses `self_contained: false` to reduce tarball size.
* Vignette images link to GitHub-hosted figures instead of bundling locally.
* Expanded test coverage for cache management, assertions, band reordering,
  and multiband reduce functions.

# vrtility 0.3.2

## New features

* Added `vrt_plan()` as a lightweight alternative to `vrt_collect()` that
  defers VRT creation for faster pipeline setup.
* Added `vrt_derived_block()` for creating derived bands using muparser
  expressions (e.g., NDVI, EVI) directly within the VRT framework.
* Added `vrt_move_band()` for reordering bands within VRT objects.
* Added VRT connection string support (`vrt://`) for efficient sub-window
  reads via `projwin`.

## Improvements

* Switched cookie file storage from `~/.cookies.txt` to
  `tools::R_user_dir("vrtility", "cache")` for CRAN compliance.
* Replaced `rappdirs` dependency with base R `tools::R_user_dir()`.
* Python environment settings now stored as package options instead of
  environment variables.
* Improved error and warning messages using `cli` throughout the package.
* Replaced `\dontrun{}` with `\donttest{}` in examples where appropriate.
* Added `@return` documentation to all exported functions.
* Added `stac_coverage_filter()` for filtering STAC results by spatial
  coverage of the area of interest.

## Bug fixes

* Fixed `hls_stac_query()` silently returning `NULL` when `max_cloud_cover`
  was specified.
* Fixed `vrt_cache_destroy()` using `options()` instead of `getOption()`,
  which returned a list instead of a character path.
* Fixed `v_assert_hls_catalog()` validation logic that never returned a
  valid result.
* Corrected COG creation options being dropped when using the `gdalraster`
  engine.

# vrtility 0.3.1

* Internal improvements and bug fixes.
* Updated vignettes to use Landsat Collection 2 Level-2 data.

