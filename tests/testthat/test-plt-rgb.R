# Tests for RGB transformations and multi-band plotting

test_that("RGB transformation functions work correctly", {
  skip_on_os("windows")

  # Test that transformation functions exist and are callable
  expect_true(exists("gamma_trans"))
  expect_true(exists("histeq_trans"))
  expect_true(exists("hist_all_trans"))

  # Test with sample data
  test_data <- runif(300, 0, 1)
  attr(test_data, "gis") <- list(dim = c(10, 10, 3)) # 10x10 image with 3 bands

  # Gamma transformation should be callable
  expect_no_error(gamma_trans(test_data))

  # Histogram equalization should be callable
  expect_no_error(histeq_trans(test_data))

  # All-band histogram equalization should be callable
  expect_no_error(hist_all_trans(test_data))
})

test_that("RGB plotting works with different transformations", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test linear RGB (default)
  vdiffr::expect_doppelganger(
    "RGB linear transformation",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "linear",
      title = "none",
      main = "Linear RGB",
      xlab = "Easting",
      ylab = "Northing"
    )
  )

  # Test gamma transformation
  vdiffr::expect_doppelganger(
    "RGB gamma transformation",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "gamma",
      title = "none",
      main = "Gamma RGB",
      xlab = "Easting",
      ylab = "Northing"
    )
  )

  # Test histogram equalization
  vdiffr::expect_doppelganger(
    "RGB histogram equalization",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "hist",
      title = "none",
      main = "Histogram Equalized RGB",
      xlab = "Easting",
      ylab = "Northing"
    )
  )

  # Test all-band histogram equalization
  vdiffr::expect_doppelganger(
    "RGB all-band histogram equalization",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "hist_all",
      title = "none",
      main = "All-band Histogram RGB",
      xlab = "Easting",
      ylab = "Northing"
    )
  )
})

test_that("RGB plotting handles percentile cuts correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test RGB with default percentile cuts
  vdiffr::expect_doppelganger(
    "RGB with default percentile cuts",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      title = "none",
      main = "RGB with Auto Percentile"
    )
  )

  # Test RGB with custom percentile cuts
  vdiffr::expect_doppelganger(
    "RGB with custom percentile cuts",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      minmax_pct_cut = c(5, 95),
      title = "none",
      main = "RGB 5-95 Percentile"
    )
  )

  # Test RGB with manual min/max
  vdiffr::expect_doppelganger(
    "RGB with manual min/max",
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      minmax_def = c(0, 0, 0, 3000, 3000, 3000),
      title = "none",
      main = "RGB Manual MinMax"
    )
  )
})

test_that("RGB plotting rejects legend correctly", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Should produce a message when trying to use legend with RGB
  expect_message(
    plot(ds, bands = c(1, 2, 3), legend = TRUE),
    "legend is not supported for RGB plot"
  )
})

test_that("single band plotting works with all color options", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test with different color palettes
  vdiffr::expect_doppelganger(
    "single band with viridis colors",
    plot_raster_src(
      s2files[1],
      bands = 4,
      col = grDevices::hcl.colors(20, "viridis"),
      title = "description",
      legend = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "single band with plasma colors",
    plot_raster_src(
      s2files[1],
      bands = 4,
      col = grDevices::hcl.colors(20, "plasma"),
      title = "description",
      legend = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "single band with custom colors",
    plot_raster_src(
      s2files[1],
      bands = 4,
      col = c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF"),
      title = "description",
      legend = TRUE
    )
  )
})

test_that("color table functionality works", {
  skip_on_os("windows")
  skip_on_ci()

  evc_file <- system.file("extdata/storml_evc.tif", package = "gdalraster")
  evc_vat <- system.file("extdata/LF20_EVC_220.csv", package = "gdalraster")
  vat <- read.csv(evc_vat)
  vat <- vat[, c(1, 6:8)]
  ds <- methods::new(gdalraster::GDALRaster, evc_file, read_only = TRUE)
  # dm <- ds$dim()
  on.exit(ds$close())
  vdiffr::expect_doppelganger(
    "plot with custom color table",
    plot(ds, bands = 1, col_tbl = vat, interpolate = FALSE)
  )
})

test_that("interpolation affects visual output appropriately", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test interpolation on vs off at low resolution
  vdiffr::expect_doppelganger("low resolution with interpolation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot(
      ds,
      bands = 1,
      interpolate = TRUE,
      xsize = 25,
      ysize = 25,
      main = "Interpolated"
    )
  })

  vdiffr::expect_doppelganger("low resolution without interpolation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot(
      ds,
      bands = 1,
      interpolate = FALSE,
      xsize = 25,
      ysize = 25,
      main = "Not Interpolated"
    )
  })
})
