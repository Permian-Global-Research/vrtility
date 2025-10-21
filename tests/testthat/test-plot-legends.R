# Comprehensive tests for legend functionality

test_that("discrete legend detection works correctly", {
  skip_on_os("windows")

  # Test discrete data detection
  discrete_data <- rep(c(1, 2, 3, 4, 5), each = 20)
  col_fn <- scales::colour_ramp(c("red", "blue", "green"))

  result <- vrtility:::.create_color_mapping(
    discrete_data,
    1,
    NULL,
    col_fn,
    TRUE,
    NULL
  )

  expect_false(result$use_normalization)
  expect_true(is.function(result$col_map_fn))

  # Test that discrete color mapping produces distinct colors
  test_values <- c(1, 2, 3, 4, 5)
  colors <- result$col_map_fn(test_values)
  expect_equal(length(unique(colors)), 5) # Should have 5 distinct colors
})

test_that("continuous legend detection works correctly", {
  skip_on_os("windows")

  # Test continuous data detection
  continuous_data <- runif(1000, 0, 100)
  col_fn <- scales::colour_ramp(c("red", "blue"))

  result <- vrtility:::.create_color_mapping(
    continuous_data,
    1,
    NULL,
    col_fn,
    TRUE,
    NULL
  )

  expect_true(result$use_normalization)
  expect_true(is.function(result$col_map_fn))
})

test_that("NoData values are properly excluded from legend calculations", {
  skip_on_os("windows")

  # Create data with NoData values
  valid_data <- c(1, 2, 3, 4, 5)
  data_with_nodata <- c(valid_data, rep(-9999, 10))
  col_fn <- scales::colour_ramp(c("red", "blue"))

  result <- vrtility:::.create_color_mapping(
    data_with_nodata,
    1,
    NULL,
    col_fn,
    TRUE,
    -9999
  )

  # Should still be treated as discrete (5 unique valid values)
  expect_false(result$use_normalization)

  # Test legend creation with NoData
  mock_xlim <- c(0, 100)
  mock_ylim <- c(0, 100)

  legend_data <- vrtility:::.create_legend(
    data_with_nodata,
    mock_xlim,
    mock_ylim,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    2,
    -9999
  )

  expect_true(legend_data$is_discrete)
  expect_equal(length(legend_data$unique_values), 5)
  expect_false(-9999 %in% legend_data$unique_values)
})

test_that("percentile cuts exclude NoData values", {
  skip_on_os("windows")

  # Create data with NoData values at extremes
  valid_data <- runif(100, 10, 90)
  data_with_nodata <- c(valid_data, rep(-9999, 20))
  col_fn <- scales::colour_ramp(c("red", "blue"))

  mock_xlim <- c(0, 100)
  mock_ylim <- c(0, 100)

  legend_data <- vrtility:::.create_legend(
    data_with_nodata,
    mock_xlim,
    mock_ylim,
    col_fn,
    NULL,
    TRUE,
    NULL,
    c(5, 95),
    1,
    "transparent",
    2,
    -9999
  )

  # Min/max should be from valid data percentiles, not including -9999
  expect_true(legend_data$mm[1] >= 10) # Should be >= 10, not -9999
  expect_true(legend_data$mm[2] <= 90) # Should be <= 90
})

test_that("legend visual tests work correctly", {
  skip_on_os("windows")
  skip_on_ci()

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ds <- methods::new(gdalraster::GDALRaster, s2files[1])
  on.exit(ds$close())

  # Test discrete legend with custom colors
  vdiffr::expect_doppelganger("discrete legend with custom colors", {
    # Create a plot that should trigger discrete legend
    plot(
      ds,
      bands = 5,
      col = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
      legend = TRUE,
      main = "Discrete Colors",
      xsize = 100,
      ysize = 100
    )
  })

  # Test continuous legend
  vdiffr::expect_doppelganger(
    "continuous legend with viridis colors",
    plot(
      ds,
      bands = 1,
      col = grDevices::hcl.colors(20, "viridis"),
      legend = TRUE,
      main = "Continuous Viridis",
      xsize = 100,
      ysize = 100
    )
  )

  # Test legend positioning with different margin configurations
  vdiffr::expect_doppelganger(
    "legend with minimal margins",
    plot(
      ds,
      bands = 1,
      legend = TRUE,
      main = "",
      xlab = "",
      ylab = "",
      xsize = 100,
      ysize = 100
    )
  )

  # Test legend with percentile cuts
  vdiffr::expect_doppelganger(
    "legend with percentile cuts",
    plot(
      ds,
      bands = 1,
      legend = TRUE,
      minmax_pct_cut = c(2, 98),
      main = "Percentile Cut Legend",
      xsize = 100,
      ysize = 100
    )
  )

  # Test legend digit formatting
  vdiffr::expect_doppelganger(
    "legend with custom digit formatting",
    plot(
      ds,
      bands = 1,
      legend = TRUE,
      digits = 0,
      main = "Integer Legend Labels",
      xsize = 100,
      ysize = 100
    )
  )
})

test_that("legend creation handles edge cases", {
  skip_on_os("windows")

  mock_xlim <- c(0, 100)
  mock_ylim <- c(0, 100)
  col_fn <- scales::colour_ramp(c("red", "blue"))

  # Test with single unique value
  single_value_data <- rep(42, 100)
  legend_data <- vrtility:::.create_legend(
    single_value_data,
    mock_xlim,
    mock_ylim,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    2,
    NULL
  )

  expect_true(legend_data$is_discrete)
  expect_equal(length(legend_data$unique_values), 1)
  expect_equal(legend_data$unique_values[1], 42)

  # Test with all NA values - should handle gracefully with no-data legend
  all_na_data <- rep(NA, 100)

  # Should not produce warnings about min/max on empty data
  expect_no_warning({
    legend_data_na <- vrtility:::.create_legend(
      all_na_data,
      mock_xlim,
      mock_ylim,
      col_fn,
      NULL,
      TRUE,
      NULL,
      NULL,
      1,
      "transparent",
      2,
      NULL
    )
  })

  # Should create a valid legend even with no data
  expect_true(legend_data_na$is_discrete)
  expect_equal(legend_data_na$n_unique, 0)
  expect_null(legend_data_na$unique_values)
  expect_true(is.matrix(legend_data_na$img))

  # Test with exactly 12 unique values (boundary case)
  boundary_data <- rep(1:12, each = 10)
  legend_data_boundary <- vrtility:::.create_legend(
    boundary_data,
    mock_xlim,
    mock_ylim,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    2,
    NULL
  )

  expect_false(legend_data_boundary$is_discrete) # Should be continuous at 12+ values
})

test_that("all-NA data plotting with legend doesn't crash", {
  skip_on_os("windows")

  # Create raster with all NA values
  all_na_data <- rep(NA_real_, 100 * 100)
  raster <- vector_to_MEM(all_na_data, nbands = 1, xsize = 100, ysize = 100)

  # This should not throw an error, specifically the seq() error in .draw_legend
  vdiffr::expect_doppelganger(
    "all NA data plot",
    plot(raster, bands = 1, legend = TRUE, main = "All NA Test")
  )

  # Test the underlying legend creation with all NA
  mock_xlim <- c(0, 100)
  mock_ylim <- c(0, 100)
  col_fn <- scales::colour_ramp(c("red", "blue"))

  legend_data <- vrtility:::.create_legend(
    all_na_data,
    mock_xlim,
    mock_ylim,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    2,
    NULL
  )

  # Should create a valid legend structure
  expect_true(legend_data$is_discrete)
  expect_equal(legend_data$n_unique, 0)
  expect_null(legend_data$unique_values)
  expect_true(is.matrix(legend_data$img))
})
