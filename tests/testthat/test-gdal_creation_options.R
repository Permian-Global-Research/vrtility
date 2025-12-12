test_that("gdal_config_options returns named vector with expected keys", {
  cfg <- gdal_config_options()
  expect_true(is.character(cfg))
  expect_true(length(cfg) > 0)
  expect_true(all(
    c("VSI_CACHE", "VSI_CACHE_SIZE", "GDAL_HTTP_MAX_RETRY") %in% names(cfg)
  ))
  expect_equal(cfg[["VSI_CACHE"]], "TRUE")
})

test_that("gdal_creation_options default contains expected creation options", {
  co <- gdal_creation_options()
  expect_true(is.character(co))
  expect_true(any(grepl("^COMPRESS=DEFLATE$", co)))
  expect_true(any(grepl("^TILED=YES$", co)))
  expect_true(any(grepl("^COPY_SRC_OVERVIEWS=YES$", co)))
})

test_that("gdal_creation_options omits COG-specific options when output_format is COG", {
  co_cog <- gdal_creation_options(output_format = "COG")
  # TILED and COPY_SRC_OVERVIEWS should be removed (NULL) for COG
  expect_false(any(grepl("^TILED=", co_cog)))
  expect_false(any(grepl("^COPY_SRC_OVERVIEWS=", co_cog)))
  # and output flag is present
  expect_equal(co_cog[1], "-of")
  expect_equal(co_cog[2], "COG")
})

test_that("gdal_creation_options cli_format emits -co prefixes in alternating pattern", {
  co_cli <- gdal_creation_options(cli_format = TRUE)
  expect_true(length(co_cli) %% 2 == 0) # should be pairs of -co and option
  odd_positions <- seq(1, length(co_cli), by = 2)
  expect_true(all(co_cli[odd_positions] == "-co"))
})

test_that("gdalwarp_options builds expected flags", {
  wo <- gdalwarp_options(
    multi = TRUE,
    warp_memory = "25%",
    num_threads = 2,
    unified_src_nodata = "PARTIAL"
  )
  expect_true("-multi" %in% wo)
  expect_true("-wm" %in% wo)
  expect_true(any(grepl("NUM_THREADS=2", wo)))
  expect_true(any(grepl("UNIFIED_SRC_NODATA=PARTIAL", wo)))
})

test_that("format_options_for_create extracts -of and remaining opts", {
  opts <- c("-of", "COG", "-co", "A=1", "-co", "B=2")
  fo <- format_options_for_create(opts)
  expect_equal(fo$fmt, "COG")
  expect_equal(fo$opts, c("-co", "A=1", "-co", "B=2"))
})


test_that("gdalwarp_options builds expected flags", {
  wo <- gdalwarp_options(
    multi = TRUE,
    warp_memory = "25%",
    num_threads = 2,
    unified_src_nodata = "PARTIAL"
  )
  expect_true("-multi" %in% wo)
  expect_true("-wm" %in% wo)
  expect_true(any(grepl("NUM_THREADS=2", wo)))
  expect_true(any(grepl("UNIFIED_SRC_NODATA=PARTIAL", wo)))
})

test_that("gdal_creation_options fails with MEM format", {
  expect_error(
    gdal_creation_options(output_format = "MEM"),
    class = "unsupported_gdal_format_error"
  )
})
