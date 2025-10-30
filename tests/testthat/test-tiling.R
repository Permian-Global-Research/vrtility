test_that("get_tiles basic functionality works", {
  # Test basic case: 100x100 image with 50x50 tiles
  result <- get_tiles(100, 100, 50, 50)

  # Should produce 4 tiles (2x2 grid)
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("nXOff", "nYOff", "nXSize", "nYSize"))

  # Check all values are integers
  expect_true(all(is.integer(result)))

  # Check offsets are 0-based (GDAL convention)
  # The function orders by x first, then y (row-wise traversal)
  expected_offsets <- matrix(
    c(
      0,
      0,
      50,
      50, # Top-left
      50,
      0,
      50,
      50, # Bottom-left
      0,
      50,
      50,
      50, # Top-right
      50,
      50,
      50,
      50 # Bottom-right
    ),
    ncol = 4,
    byrow = TRUE
  )
  colnames(expected_offsets) <- c("nXOff", "nYOff", "nXSize", "nYSize")

  expect_equal(result, expected_offsets)
})

test_that("get_tiles handles edge cases correctly", {
  # Test when image dimensions don't divide evenly by window size
  # 100 rows, 90 cols, 30x40 windows -> should give 3x3 = 9 tiles
  result <- get_tiles(100, 90, 30, 40)

  # Should produce 9 tiles (3 rows x 3 cols)
  expect_equal(nrow(result), 9)

  # Check that last tiles have correct sizes (handle remainder)
  # Last x-position should have size 20 (100 - 80)
  last_x_tiles <- result[result[, "nXOff"] == 80, ]
  expect_equal(unique(last_x_tiles[, "nXSize"]), 20)

  # Last y-position should have size 30 (90 - 60)
  last_y_tiles <- result[result[, "nYOff"] == 60, ]
  expect_equal(unique(last_y_tiles[, "nYSize"]), 30)
})

test_that("get_tiles handles overlap correctly", {
  # Test with overlap
  result <- get_tiles(100, 100, 40, 40, overlap = 10)

  # Should still produce 9 tiles (3x3) but with different sizes
  expect_equal(nrow(result), 9)

  # Non-edge tiles should have size = window + overlap
  non_edge_tiles <- result[result[, "nXOff"] < 80 & result[, "nYOff"] < 80, ]
  expect_true(all(non_edge_tiles[, "nXSize"] == 50)) # 40 + 10
  expect_true(all(non_edge_tiles[, "nYSize"] == 50)) # 40 + 10
})

test_that("get_tiles handles single tile case", {
  # Test when window size >= image size
  result <- get_tiles(50, 60, 100, 100)

  # Should produce exactly 1 tile
  expect_equal(nrow(result), 1)
  expect_equal(
    result[1, ],
    c(nXOff = 0L, nYOff = 0L, nXSize = 50L, nYSize = 60L)
  )
})

test_that("get_tiles handles minimum input values", {
  # Test smallest possible values
  result <- get_tiles(1, 1, 1, 1)

  expect_equal(nrow(result), 1)
  expect_equal(result[1, ], c(nXOff = 0L, nYOff = 0L, nXSize = 1L, nYSize = 1L))
})

test_that("get_tiles input validation works", {
  # Test that inputs are converted to integers
  result1 <- get_tiles(100.5, 100.7, 50.2, 50.9)
  result2 <- get_tiles(100L, 100L, 50L, 50L)

  expect_equal(result1, result2)

  # Test with zero overlap
  result_no_overlap <- get_tiles(100, 100, 50, 50, overlap = 0)
  expect_equal(nrow(result_no_overlap), 4)
})

test_that("get_tiles produces valid GDAL RasterIO parameters", {
  result <- get_tiles(1000, 800, 256, 256)

  # All offsets should be >= 0
  expect_true(all(result[, "nXOff"] >= 0))
  expect_true(all(result[, "nYOff"] >= 0))

  # All sizes should be > 0
  expect_true(all(result[, "nXSize"] > 0))
  expect_true(all(result[, "nYSize"] > 0))

  # No tile should extend beyond image boundaries
  expect_true(all(result[, "nXOff"] + result[, "nXSize"] <= 1000))
  expect_true(all(result[, "nYOff"] + result[, "nYSize"] <= 800))
})

test_that("get_tiles coverage is complete and non-overlapping (without overlap param)", {
  img_rows <- 123
  img_cols <- 87
  x_window <- 30
  y_window <- 25

  result <- get_tiles(img_rows, img_cols, x_window, y_window)

  # Create coverage matrix to check no gaps/overlaps
  coverage <- matrix(FALSE, nrow = img_rows, ncol = img_cols)

  for (i in seq_len(nrow(result))) {
    x_start <- result[i, "nXOff"] + 1 # Convert back to 1-based for R indexing
    y_start <- result[i, "nYOff"] + 1
    x_end <- x_start + result[i, "nXSize"] - 1
    y_end <- y_start + result[i, "nYSize"] - 1

    # Check no overlap (all should be FALSE before setting to TRUE)
    expect_true(all(!coverage[x_start:x_end, y_start:y_end]))

    # Mark as covered
    coverage[x_start:x_end, y_start:y_end] <- TRUE
  }

  # Check complete coverage (all should be TRUE)
  expect_true(all(coverage))
})

test_that("get_tiles with overlap produces correct overlapping regions", {
  result <- get_tiles(100, 100, 30, 30, overlap = 5)

  # With 30x30 windows and 5 pixel overlap, step size is 30
  # This creates a 4x4 grid = 16 tiles (not 9)
  expect_equal(nrow(result), 16)

  # Check that non-edge tiles have size = window + overlap = 35
  non_edge_tiles <- result[result[, "nXOff"] < 90 & result[, "nYOff"] < 90, ]
  expect_true(all(non_edge_tiles[, "nXSize"] == 35)) # 30 + 5
  expect_true(all(non_edge_tiles[, "nYSize"] == 35)) # 30 + 5

  # Check that tiles actually overlap
  # Find two adjacent tiles and verify overlap
  tile1 <- result[1, ] # First tile at (0,0)
  tile2 <- result[2, ] # Second tile at (30,0) - should be adjacent

  # tile1 should extend from 0 to 35, tile2 should start at 30
  # So they overlap from 30 to 35
  tile1_x_end <- tile1["nXOff"] + tile1["nXSize"]
  tile2_x_start <- tile2["nXOff"]

  expect_gt(tile1_x_end, tile2_x_start) # Should overlap
  expect_equal(as.numeric(tile1_x_end - tile2_x_start), 5) # Overlap should be 5 pixels
})

test_that("get_tiles performance characteristics", {
  # Test that function completes reasonably quickly for large inputs
  start_time <- Sys.time()
  result <- get_tiles(10000, 8000, 512, 512)
  end_time <- Sys.time()

  # Should complete in under 1 second for reasonable inputs
  expect_lt(as.numeric(end_time - start_time), 1.0)

  # Should produce reasonable number of tiles
  expected_tiles <- ceiling(10000 / 512) * ceiling(8000 / 512)
  expect_equal(nrow(result), expected_tiles)
})

test_that("get_tiles matrix structure is correct", {
  result <- get_tiles(200, 150, 64, 48)

  # Check matrix properties
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("nXOff", "nYOff", "nXSize", "nYSize"))

  # Check that all values are non-negative integers
  expect_true(all(result >= 0))
  expect_true(all(result == as.integer(result)))
})
