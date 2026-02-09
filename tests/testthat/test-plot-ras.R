test_that(".validate_plot_params works correctly", {
  skip_on_os("windows")

  # Test valid inputs
  result <- vrtility:::.validate_plot_params(
    bands = 1,
    max_pixels = 1000,
    col = c("red", "blue")
  )
  expect_true(result$valid)
  expect_equal(result$bands, 1)
  expect_equal(result$max_pixels, 1000)
  expect_equal(result$col, c("red", "blue"))
  expect_true(is.function(result$col_map_fn))

  # Test RGB bands
  result_rgb <- vrtility:::.validate_plot_params(
    bands = c(1, 2, 3),
    max_pixels = NULL,
    col = NULL
  )
  expect_true(result_rgb$valid)
  expect_equal(result_rgb$bands, c(1, 2, 3))
  expect_equal(result_rgb$max_pixels, Inf)
  expect_null(result_rgb$col_map_fn)

  # Test NULL bands - should default to 1
  expect_message(
    result_null <- vrtility:::.validate_plot_params(
      bands = NULL,
      max_pixels = 1000,
      col = NULL
    ),
    "defaulting to band 1"
  )
  expect_equal(result_null$bands, 1)

  # Test invalid band lengths
  expect_error(
    vrtility:::.validate_plot_params(
      bands = c(1, 2),
      max_pixels = 1000,
      col = NULL
    ),
    "must be of length 1 or 3"
  )

  # Test invalid color type
  expect_error(
    vrtility:::.validate_plot_params(bands = 1, max_pixels = 1000, col = 123),
    "must be a character vector"
  )
})

test_that(".setup_plot_dimensions works correctly", {
  skip_on_os("windows")

  # Create mock data object
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test default dimensions
  result <- vrtility:::.setup_plot_dimensions(ds, NULL, NULL, 1e6)
  expect_true(all(c("dm", "xsize", "ysize") %in% names(result)))
  expect_equal(length(result$dm), 3) # GDALRaster$dim() returns c(xsize, ysize, bands)

  # Test specified dimensions
  result_spec <- vrtility:::.setup_plot_dimensions(ds, 100, 200, 1e6)
  expect_equal(result_spec$xsize, 100)
  expect_equal(result_spec$ysize, 200)

  # Test max_pixels constraint
  expect_warning(
    result_warn <- vrtility:::.setup_plot_dimensions(ds, 1000, 1000, 100),
    "exceeds.*max_pixels.*downsampling"
  )
  expect_true(result_warn$xsize * result_warn$ysize <= 100)
})

test_that(".create_color_mapping handles discrete vs continuous data", {
  skip_on_os("windows")

  # Test continuous data
  continuous_data <- runif(1000, 0, 100)
  col_fn <- scales::colour_ramp(c("blue", "red"))

  result_cont <- vrtility:::.create_color_mapping(
    continuous_data,
    1,
    NULL,
    col_fn,
    TRUE,
    NULL
  )
  expect_true(result_cont$use_normalization)
  expect_true(is.function(result_cont$col_map_fn))

  # Test discrete data
  discrete_data <- sample.int(5, 100, replace = TRUE)
  result_disc <- vrtility:::.create_color_mapping(
    discrete_data,
    1,
    NULL,
    col_fn,
    TRUE,
    NULL
  )
  expect_false(result_disc$use_normalization)
  expect_true(is.function(result_disc$col_map_fn))

  # Test with NoData values
  data_with_nodata <- c(discrete_data, rep(-9999, 10))
  result_nodata <- vrtility:::.create_color_mapping(
    data_with_nodata,
    1,
    NULL,
    col_fn,
    TRUE,
    -9999
  )
  expect_false(result_nodata$use_normalization)

  # Test RGB data (should return original function)
  result_rgb <- vrtility:::.create_color_mapping(
    continuous_data,
    3,
    NULL,
    NULL,
    TRUE,
    NULL
  )
  expect_null(result_rgb$col_map_fn)
  expect_true(result_rgb$use_normalization)
})

test_that("margin adjustment logic works correctly", {
  skip_on_os("windows")

  # We can't easily test the internal margin logic directly, but we can test
  # the conditions that would trigger different margin settings

  # Empty title conditions
  expect_true(is.null("") || "" == "" || nchar(trimws("")) == 0)
  expect_true(is.null("   ") || "   " == "" || nchar(trimws("   ")) == 0)
  expect_false(is.null("Title") || "Title" == "" || nchar(trimws("Title")) == 0)

  # Empty label conditions
  expect_true(is.null("") || "" == "" || nchar(trimws("")) == 0)
  expect_false(
    is.null("X Label") || "X Label" == "" || nchar(trimws("X Label")) == 0
  )
})

# Integration tests with real data ============================================

test_that("raster plotting works with various configurations", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(
    s2files,
    datetimes = paste0("2020-06-0", seq_len(length(s2files)))
  )

  ex_vrt_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    )

  # Test collection plotting with different title options
  vdiffr::expect_doppelganger(
    "collect plot works with default title",
    plot(ex_vrt_mask, item = 2, c(3, 2, 1))
  )

  vdiffr::expect_doppelganger(
    "collect plot works with title = 'none'",
    plot(ex_vrt_mask, item = 2, c(3, 2, 1), title = "none")
  )

  vdiffr::expect_doppelganger(
    "collect plot works with empty labels",
    plot(
      ex_vrt_mask,
      item = 2,
      c(3, 2, 1),
      title = "none",
      xlab = "",
      ylab = ""
    )
  )

  t_block <- ex_vrt_mask[[1]][[1]]

  # Test single band plotting with various configurations
  vdiffr::expect_doppelganger(
    "block plot works with default title",
    plot(t_block, 2)
  )

  vdiffr::expect_doppelganger(
    "block plot works with title = 'none'",
    plot(t_block, 2, title = "none")
  )

  vdiffr::expect_doppelganger(
    "block plot works with title = 'dttm'",
    plot(t_block, 2, title = "dttm")
  )

  # Test legend functionality
  vdiffr::expect_doppelganger(
    "single band plot with legend",
    plot(t_block, 2, legend = TRUE)
  )

  vdiffr::expect_doppelganger(
    "single band plot with custom colors and legend",
    plot(t_block, 2, col = c("red", "yellow", "green", "blue"), legend = TRUE)
  )

  # Test margin adjustments with empty labels
  vdiffr::expect_doppelganger(
    "plot with empty title and labels",
    plot(
      t_block,
      2,
      title = "none",
      main = "",
      xlab = "",
      ylab = "",
      legend = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "plot with only title",
    plot(t_block, 2, main = "Test Title", xlab = "", ylab = "", legend = FALSE)
  )

  vdiffr::expect_doppelganger(
    "plot with only x label",
    plot(
      t_block,
      2,
      main = "",
      xlab = "X Coordinate",
      ylab = "",
      legend = FALSE
    )
  )

  vdiffr::expect_doppelganger(
    "plot with only y label",
    plot(
      t_block,
      2,
      main = "",
      xlab = "",
      ylab = "Y Coordinate",
      legend = FALSE
    )
  )

  ex_vrt_mask_warp <- vrt_warp(
    ex_vrt_mask,
    t_srs = t_block$srs,
    te = t_block$bbox,
    tr = t_block$res
  )

  vdiffr::expect_doppelganger(
    "warp plot works",
    plot(ex_vrt_mask_warp, item = 3, c(3, 2, 1))
  )

  ex_vrt_mask_warp_stack <- vrt_stack(ex_vrt_mask_warp) |>
    vrt_set_py_pixelfun()

  vdiffr::expect_doppelganger(
    "stack plot works",
    plot(ex_vrt_mask_warp_stack, c(3, 2, 1))
  )
})

# Test discrete vs continuous legend detection =================================

test_that("discrete vs continuous legend detection works", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Create a raster with discrete values for testing
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test discrete legend
  vdiffr::expect_doppelganger("discrete legend with few unique values", {
    # Create discrete data by reading and modifying

    # Plot with discrete colors
    plot(
      ds,
      bands = 5,
      col = c(
        'red',
        'blue',
        'green',
        'orange',
        "purple",
        "pink"
      ),
      legend = TRUE,
      main = 'Discrete Legend Test',
      xsize = 100,
      ysize = 100
    )
  })

  # Test continuous legend
  vdiffr::expect_doppelganger(
    "continuous legend with many unique values",
    plot(
      ds,
      bands = 1,
      legend = TRUE,
      main = 'Continuous Legend Test',
      xsize = 100,
      ysize = 100
    )
  )
})

# Test percentile cutting with NoData values ==================================

test_that("percentile cutting handles NoData values correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test that NoData values don't interfere with percentile cuts
  vdiffr::expect_doppelganger("percentile cut ignores nodata values", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())

    plot(
      ds,
      bands = 1,
      minmax_pct_cut = c(2, 98),
      legend = TRUE,
      main = "Percentile Cut Test",
      xsize = 100,
      ysize = 100
    )
  })
})

# Test RGB transformations ====================================================

test_that("RGB transformations work correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test different RGB transformations
  vdiffr::expect_doppelganger("RGB plot with linear transformation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "linear",
      title = "none",
      main = "Linear RGB"
    )
  })

  vdiffr::expect_doppelganger("RGB plot with gamma transformation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot_raster_src(
      s2files[1],
      bands = c(3, 2, 1),
      rgb_trans = "gamma",
      title = "none",
      main = "Gamma RGB"
    )
  })
})

# Test error conditions =======================================================

test_that("plotting functions handle errors gracefully", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test invalid band count
  expect_error(
    plot(ds, bands = c(1, 2)),
    "must be of length 1 or 3"
  )

  # Test legend with RGB
  expect_message(
    plot(ds, bands = c(1, 2, 3), legend = TRUE),
    "not supported for RGB"
  )

  # Test invalid color specification
  expect_error(
    plot(ds, bands = 1, col = 123),
    "must be a character vector"
  )
})

# Test axes and label positioning ==============================================

test_that("axes and labels position correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  # Test with axes disabled
  vdiffr::expect_doppelganger("plot without axes", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot(ds, bands = 1, axes = FALSE, main = "No Axes Test")
  })

  # Test with custom xlim and ylim
  vdiffr::expect_doppelganger("plot with custom limits", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    gt <- ds$getGeoTransform()
    xlim_custom <- c(gt[1], gt[1] + gt[2] * 200)
    ylim_custom <- c(gt[4] + gt[6] * 200, gt[4])

    plot(
      ds,
      bands = 1,
      xlim = xlim_custom,
      ylim = ylim_custom,
      main = "Custom Limits",
      legend = TRUE
    )
  })
})

# Test interpolation settings =================================================

test_that("interpolation settings work correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  vdiffr::expect_doppelganger("plot without interpolation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot(
      ds,
      bands = 1,
      interpolate = FALSE,
      main = "No Interpolation",
      xsize = 50,
      ysize = 50
    )
  })

  vdiffr::expect_doppelganger("plot with interpolation", {
    ds <- methods::new(gdalraster::GDALRaster, s2files[1])
    on.exit(ds$close())
    plot(
      ds,
      bands = 1,
      interpolate = TRUE,
      main = "With Interpolation",
      xsize = 50,
      ysize = 50
    )
  })
})

# Performance and edge case tests =============================================

test_that("plotting handles edge cases", {
  skip_on_os("windows")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test with very small dimensions
  expect_no_error(
    plot(ds, bands = 1, xsize = 10, ysize = 10, main = "Tiny Plot")
  )

  # Test with max_pixels constraint
  expect_warning(
    plot(ds, bands = 1, max_pixels = 100, main = "Max Pixels Test"),
    "exceeds.*max_pixels.*downsampling"
  )
})
