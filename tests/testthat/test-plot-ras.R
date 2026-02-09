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

  # Test collection plotting
  vdiffr::expect_doppelganger(
    "collect plot works with default title",
    plot(ex_vrt_mask, item = 2, c(3, 2, 1))
  )

  t_block <- ex_vrt_mask[[1]][[1]]

  # Test block plotting with legend
  vdiffr::expect_doppelganger(
    "single band plot with legend",
    plot(t_block, 2, legend = TRUE)
  )

  # Test warp plotting
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

  # Test stack plotting
  ex_vrt_mask_warp_stack <- vrt_stack(ex_vrt_mask_warp) |>
    vrt_set_py_pixelfun()

  vdiffr::expect_doppelganger(
    "stack plot works",
    plot(ex_vrt_mask_warp_stack, c(3, 2, 1))
  )
})

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
