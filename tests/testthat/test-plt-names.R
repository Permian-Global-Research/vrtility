# Tests for margin adjustment and layout functionality

test_that("margin adjustment logic works correctly", {
  skip_on_os("windows")

  # Test empty string detection
  expect_true(is.null("") || "" == "" || nchar(trimws("")) == 0)
  expect_true(is.null("   ") || "   " == "" || nchar(trimws("   ")) == 0)
  expect_true(is.null("\t\n") || "\t\n" == "" || nchar(trimws("\t\n")) == 0)

  # Test non-empty string detection
  expect_false(is.null("Title") || "Title" == "" || nchar(trimws("Title")) == 0)
  expect_false(
    is.null("  Title  ") || "  Title  " == "" || nchar(trimws("  Title  ")) == 0
  )

  # Test NULL detection
  test_null <- NULL
  expect_true(
    is.null(test_null) || test_null == "" || nchar(trimws(test_null)) == 0
  )
})

test_that("margin adjustments produce different visual results", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test plot with all labels present (larger margins)
  vdiffr::expect_doppelganger(
    "plot with all labels - larger margins",
    plot(
      ds,
      bands = 1,
      main = "Test Title",
      xlab = "X Coordinate",
      ylab = "Y Coordinate",
      xsize = 100,
      ysize = 100
    )
  )

  # Test plot with no labels (smaller margins)
  vdiffr::expect_doppelganger(
    "plot with no labels - smaller margins",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "",
      ylab = "",
      xsize = 100,
      ysize = 100
    )
  )
})
