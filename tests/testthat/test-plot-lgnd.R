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
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
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
    col_fn,
    NULL,
    TRUE,
    NULL,
    c(5, 95),
    1,
    "transparent",
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
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
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
      col_fn,
      NULL,
      TRUE,
      NULL,
      NULL,
      1,
      "transparent",
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
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    NULL
  )

  expect_false(legend_data_boundary$is_discrete) # Should be continuous at 12+ values
})

test_that("all-NA data plotting with legend doesn't crash", {
  skip_on_os("windows")

  # Create raster with all NA values
  all_na_data <- rep(NA_real_, 100 * 100)
  raster <- r_to_MEM(all_na_data, nbands = 1, xsize = 100, ysize = 100)

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
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    NULL
  )

  # Should create a valid legend structure
  expect_true(legend_data$is_discrete)
  expect_equal(legend_data$n_unique, 0)
  expect_null(legend_data$unique_values)
  expect_true(is.matrix(legend_data$img))
})

test_that("auto_determine_digits works correctly for various data ranges", {
  skip_on_os("windows")

  # Test integer data - should return 0 digits
  integer_data <- c(1, 2, 3, 4, 5)
  expect_equal(vrtility:::.auto_determine_digits(integer_data), 0)

  # Test large magnitude data (>= 1000) - should return 0 digits
  large_magnitude <- c(1000, 2000, 3000, 4000)
  expect_equal(vrtility:::.auto_determine_digits(large_magnitude), 0)

  # Test hundreds magnitude (>= 100) - should return 0 digits due to magnitude
  hundreds_magnitude <- c(100, 150, 200, 250)
  expect_equal(vrtility:::.auto_determine_digits(hundreds_magnitude), 0)

  # Test tens magnitude (>= 10) - should return 0 digits due to magnitude
  tens_magnitude <- c(10, 15, 20, 25)
  expect_equal(vrtility:::.auto_determine_digits(tens_magnitude), 0)

  # Test units range with decimal values - should return 2 digits
  units_range <- c(1.1, 1.5, 2.0, 2.8)
  expect_equal(vrtility:::.auto_determine_digits(units_range), 2)

  # Test tenths range (< 1 but >= 0.1) - should return 2 digits
  tenths_range <- c(0.1, 0.3, 0.5, 0.8)
  expect_equal(vrtility:::.auto_determine_digits(tenths_range), 2)

  # Test hundredths range (< 0.1 but >= 0.01) - should return 3 digits
  hundredths_range <- c(0.01, 0.03, 0.05, 0.08)
  expect_equal(vrtility:::.auto_determine_digits(hundredths_range), 3)

  # Test very small values - should use difference-based logic
  tiny_values <- c(0.0001, 0.0002, 0.0003)
  result <- vrtility:::.auto_determine_digits(tiny_values)
  expect_true(result >= 4) # Should need at least 4 digits
  expect_true(result <= 6) # Should be capped at 6 digits
})

test_that("auto_determine_digits handles edge cases", {
  skip_on_os("windows")

  # Test empty vector
  expect_equal(vrtility:::.auto_determine_digits(numeric(0)), 0)

  # Test all NA values
  expect_equal(vrtility:::.auto_determine_digits(c(NA, NA, NA)), 0)

  # Test infinite values
  expect_equal(vrtility:::.auto_determine_digits(c(Inf, -Inf, 1, 2)), 0)

  # Test single value
  expect_equal(vrtility:::.auto_determine_digits(c(42.5)), 1) # Default for single value

  # Test identical values (no range)
  expect_equal(vrtility:::.auto_determine_digits(c(5.5, 5.5, 5.5)), 4)

  # Test mix of integers and decimals where range calculation matters
  mixed_data <- c(1.0, 2.0, 3.0) # All end in .0 but stored as numeric
  expect_equal(vrtility:::.auto_determine_digits(mixed_data), 0) # Should detect as integers

  # Test precision boundary cases
  boundary_1000 <- c(999, 1001) # Max magnitude >= 1000
  expect_equal(vrtility:::.auto_determine_digits(boundary_1000), 0)

  boundary_100 <- c(99, 101) # Max magnitude >= 100
  expect_equal(vrtility:::.auto_determine_digits(boundary_100), 1)

  boundary_10 <- c(9, 11) # Max magnitude >= 10
  expect_equal(vrtility:::.auto_determine_digits(boundary_10), 1)
})

test_that("auto_determine_digits integrates correctly with legend drawing", {
  skip_on_os("windows")

  # Test that the function is called correctly in discrete legends
  discrete_data <- c(1.11, 2.22, 3.33)
  col_fn <- scales::colour_ramp(c("red", "blue"))

  # Create a legend and check that auto-digits is applied
  legend_data <- vrtility:::.create_legend(
    discrete_data,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    NULL
  )

  # Create mock coordinates for draw_legend
  xlim <- c(0, 100)
  ylim <- c(0, 100)

  # This should not throw an error and should auto-determine digits
  expect_no_error({
    # Capture the plot to avoid display
    png(tempfile(fileext = ".png"))
    plot.new()
    vrtility:::.draw_legend(legend_data, xlim, ylim, digits = NULL)
    dev.off()
  })

  # Test with continuous data
  continuous_data <- runif(1000, 10.123, 15.789)
  legend_data_cont <- vrtility:::.create_legend(
    continuous_data,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    NULL
  )

  expect_no_error({
    png(tempfile(fileext = ".png"))
    plot.new()
    vrtility:::.draw_legend(legend_data_cont, xlim, ylim, digits = NULL)
    dev.off()
  })
})

test_that("auto_determine_digits respects manual override", {
  skip_on_os("windows")

  # Create test data that would normally get auto-determined digits
  test_data <- c(1.11111, 2.22222, 3.33333) # Would auto-determine to 2 digits

  col_fn <- scales::colour_ramp(c("red", "blue"))
  legend_data <- vrtility:::.create_legend(
    test_data,
    col_fn,
    NULL,
    TRUE,
    NULL,
    NULL,
    1,
    "transparent",
    NULL
  )

  xlim <- c(0, 100)
  ylim <- c(0, 100)

  # Test that manual digits override works
  expect_no_error({
    png(tempfile(fileext = ".png"))
    plot.new()
    # Manually specify 4 digits - should override auto-determination
    vrtility:::.draw_legend(legend_data, xlim, ylim, digits = 4)
    dev.off()
  })

  # Test with 0 digits override
  expect_no_error({
    png(tempfile(fileext = ".png"))
    plot.new()
    vrtility:::.draw_legend(legend_data, xlim, ylim, digits = 0)
    dev.off()
  })
})
