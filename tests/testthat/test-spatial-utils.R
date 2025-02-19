test_that("validate_bbox handles valid bounding boxes", {
  # Test valid bbox
  bbox <- c(130.8, -11.5, 131.0, -11.3)
  expect_no_error(validate_bbox(bbox))
  expect_equal(validate_bbox(bbox), bbox)

  # Test bbox at extremes
  extreme_bbox <- c(-180, -90, 180, 90)
  expect_no_error(validate_bbox(extreme_bbox))
  expect_equal(validate_bbox(extreme_bbox), extreme_bbox)
})

test_that("validate_bbox errors on invalid input types", {
  # Test non-numeric input
  expect_error(
    validate_bbox(c("130.8", "-11.5", "131.0", "-11.3")),
    class = "vrtility_type_error"
  )

  # Test wrong length
  expect_error(
    validate_bbox(c(130.8, -11.5, 131.0)),
    class = "vrtility_length_error"
  )
})

test_that("validate_bbox on invalid coordinates", {
  # Test bbox outside valid bounds
  expect_error(
    validate_bbox(c(-181, -91, 181, 91)),
    class = "vrtility_bbox_not_within_bounds"
  )

  # Test reversed coordinates (invalid polygon)
  expect_equal(
    validate_bbox(c(131.0, -11.3, 130.8, -11.5)),
    c(130.8, -11.5, 131.0, -11.3)
  )

  # Test min > max
  expect_error(
    validate_bbox(c(130.8, -11.3, 130.8, -11.5)),
    class = "vrtility_bbox_not_valid"
  )
})

test_that("validate_bbox handles edge cases", {
  # Test zero-width bbox
  zero_width <- c(130.8, -11.5, 130.8, -11.3)
  expect_error(
    validate_bbox(zero_width),
    class = "vrtility_bbox_not_valid"
  )

  # Test zero-height bbox
  zero_height <- c(130.8, -11.5, 131.0, -11.5)
  expect_error(
    validate_bbox(zero_height),
    class = "vrtility_bbox_not_valid"
  )

  # Test point (zero width and height)
  point_bbox <- c(130.8, -11.5, 130.8, -11.5)
  expect_error(
    validate_bbox(point_bbox),
    class = "vrtility_bbox_not_valid"
  )
})
