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

