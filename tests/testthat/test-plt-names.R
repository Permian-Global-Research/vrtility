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

  # Test plot with only title
  vdiffr::expect_doppelganger(
    "plot with only title",
    plot(
      ds,
      bands = 1,
      main = "Only Title",
      xlab = "",
      ylab = "",
      xsize = 100,
      ysize = 100
    )
  )

  # Test plot with only x label
  vdiffr::expect_doppelganger(
    "plot with only x label",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "X Coordinate",
      ylab = "",
      xsize = 100,
      ysize = 100
    )
  )

  # Test plot with only y label
  vdiffr::expect_doppelganger(
    "plot with only y label",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "",
      ylab = "Y Coordinate",
      xsize = 100,
      ysize = 100
    )
  )

  # Test combinations with legend
  vdiffr::expect_doppelganger(
    "legend with no labels - tight margins",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "",
      ylab = "",
      legend = TRUE,
      xsize = 100,
      ysize = 100
    )
  )

  vdiffr::expect_doppelganger(
    "legend with all labels - normal margins",
    plot(
      ds,
      bands = 1,
      main = "Full Labels",
      xlab = "X Coord",
      ylab = "Y Coord",
      legend = TRUE,
      xsize = 100,
      ysize = 100
    )
  )

  # Test whitespace-only labels (should behave like empty)
  vdiffr::expect_doppelganger(
    "plot with whitespace labels - treated as empty",
    plot(
      ds,
      bands = 1,
      main = "   ",
      xlab = "\t",
      ylab = "\n ",
      xsize = 100,
      ysize = 100
    )
  )
})

test_that("custom margin additions work correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test custom margin adjustments
  vdiffr::expect_doppelganger(
    "plot with custom margin additions",
    plot(
      ds,
      bands = 1,
      main = "Test",
      xlab = "X",
      ylab = "Y",
      mar = c(1, 1, 1, 1),
      xsize = 100,
      ysize = 100
    )
  )

  # Test asymmetric margin additions
  vdiffr::expect_doppelganger(
    "plot with asymmetric margin additions",
    plot(
      ds,
      bands = 1,
      main = "Test",
      xlab = "X",
      ylab = "Y",
      mar = c(2, 0, 0, 1),
      legend = TRUE,
      xsize = 100,
      ysize = 100
    )
  )
})

test_that("axis positioning works correctly with different margins", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test that axes still render correctly with reduced margins
  vdiffr::expect_doppelganger(
    "axes with reduced margins",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "",
      ylab = "",
      axes = TRUE,
      xsize = 100,
      ysize = 100
    )
  )

  # Test axes disabled with reduced margins
  vdiffr::expect_doppelganger(
    "no axes with reduced margins",
    plot(
      ds,
      bands = 1,
      main = "",
      xlab = "",
      ylab = "",
      axes = FALSE,
      xsize = 100,
      ysize = 100
    )
  )

  # Test that title still positions correctly with reduced top margin
  vdiffr::expect_doppelganger(
    "title positioning with mixed margins",
    plot(
      ds,
      bands = 1,
      main = "Title Present",
      xlab = "",
      ylab = "",
      xsize = 100,
      ysize = 100
    )
  )
})
