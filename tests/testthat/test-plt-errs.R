# Tests for error handling and edge cases

test_that("input validation works correctly", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test invalid band specifications
  expect_error(
    plot(ds, bands = c(1, 2)),
    "bands argument must be of length 1 or 3"
  )

  expect_error(
    plot(ds, bands = c(1, 2, 3, 4)),
    "bands argument must be of length 1 or 3"
  )

  expect_error(
    plot(ds, bands = character(0)),
    "bands argument must be of length 1 or 3"
  )

  # Test invalid color specifications
  expect_error(
    plot(ds, bands = 1, col = 123),
    "'col' must be a character string"
  )

  expect_error(
    plot(ds, bands = 1, col = list("red", "blue")),
    "'col' must be a character string"
  )

  # Test that NULL bands gets default message
  expect_message(
    plot(ds, bands = NULL),
    "bands argument is NULL, defaulting to band 1"
  )
})

test_that("device capability checking works", {
  skip_on_os("windows")

  # We can't easily mock device capabilities, but we can test the logic
  # This mainly tests that the function doesn't crash on capability checks

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Should work on normal devices
  expect_no_error(plot(ds, bands = 1, main = "Device Test"))
})

test_that("max_pixels constraint works correctly", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test that max_pixels produces warning and downsampling
  expect_warning(
    plot(ds, bands = 1, max_pixels = 100),
    "'xsize \\* ysize' exceeds 'max_pixels', downsampling applied"
  )

  # Test that reasonable max_pixels doesn't produce warning
  expect_no_warning(
    plot(ds, bands = 1, max_pixels = 1e6, xsize = 100, ysize = 100)
  )
})

test_that("coordinate and extent handling works", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test custom xlim/ylim
  gt <- ds$getGeoTransform()
  custom_xlim <- c(gt[1], gt[1] + gt[2] * 100)
  custom_ylim <- c(gt[4] + gt[6] * 100, gt[4])

  expect_no_error(
    plot(ds, bands = 1, xlim = custom_xlim, ylim = custom_ylim)
  )

  # Test that invalid extents don't crash
  expect_no_error(
    plot(ds, bands = 1, xlim = c(0, -100), ylim = c(100, 0))
  )
})

test_that("data type handling works correctly", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test different data types can be plotted
  expect_no_error(plot(ds, bands = 1, main = "Data Type Test"))

  # Test with scaling
  expect_no_error(plot(ds, bands = 1, scale_values = TRUE))
  expect_no_error(plot(ds, bands = 1, scale_values = FALSE))
})

test_that("legend edge cases are handled", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test legend with RGB produces message
  expect_message(
    plot(ds, bands = c(1, 2, 3), legend = TRUE),
    "legend is not supported for RGB plot"
  )

  # Test legend with very small plot
  expect_no_error(
    plot(ds, bands = 1, legend = TRUE, xsize = 10, ysize = 10)
  )

  # Test legend with extreme digit settings
  expect_no_error(
    plot(ds, bands = 1, legend = TRUE, digits = 0)
  )

  expect_no_error(
    plot(ds, bands = 1, legend = TRUE, digits = 10)
  )
})

test_that("pixel function handling works", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test valid pixel function
  log_transform <- function(x) log(x + 1)
  expect_no_error(
    plot(ds, bands = 1, pixel_fn = log_transform, main = "Log Transform")
  )

  # Test invalid pixel function
  expect_error(
    plot(ds, bands = 1, pixel_fn = "not_a_function"),
    "'pixel_fn' must be a function"
  )

  # Test pixel function that returns complex (should error)
  complex_fn <- function(x) complex(real = x, imaginary = x)
  expect_error(
    plot(ds, bands = 1, pixel_fn = complex_fn),
    "specify 'pixel_fn' when plotting complex data types"
  )
})

test_that("NA and nodata handling works correctly", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test custom NA color
  expect_no_error(
    plot(ds, bands = 1, na_col = "red", main = "Red NA")
  )

  expect_no_error(
    plot(ds, bands = 1, na_col = "transparent", main = "Transparent NA")
  )

  # Test that NoData values are properly detected and handled
  nodata_val <- ds$getNoDataValue(1)
  if (!is.na(nodata_val)) {
    expect_no_error(
      plot(ds, bands = 1, main = paste("NoData:", nodata_val))
    )
  }
})

test_that("south-up raster handling works", {
  skip_on_os("windows")

  # This is difficult to test without a south-up raster file
  # But we can at least verify the function doesn't crash with normal rasters

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Check geotransform to see if it's south-up
  gt <- ds$getGeoTransform()
  expect_no_error(plot(ds, bands = 1, main = "Geotransform Test"))
})

test_that("memory and performance edge cases", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test very small dimensions
  expect_no_error(
    plot(ds, bands = 1, xsize = 1, ysize = 1, main = "1x1 pixel")
  )

  # Test with minimal legend
  expect_no_error(
    plot(
      ds,
      bands = 1,
      xsize = 5,
      ysize = 5,
      legend = TRUE,
      main = "Tiny with legend"
    )
  )

  # Test with NULL max_pixels (should default to Inf)
  expect_no_error(
    plot(ds, bands = 1, max_pixels = NULL, xsize = 10, ysize = 10)
  )
})
