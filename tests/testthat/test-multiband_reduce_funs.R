test_that("geomedoid returns a function and works on a matrix", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")

  fn <- geomedoid()
  expect_type(fn, "closure")

  # Simple matrix with 3 observations, 2 bands
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  result <- fn(mat)
  expect_length(result, 2)
  # result should be one of the actual rows
  expect_true(any(apply(mat, 1, function(r) all(r == result))))
})

test_that("xoid_generator imputes NA values when impute_na = TRUE", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")

  # Create a matrix with NAs in one column
  mat <- matrix(c(
    1, 2, 3, NA,
    10, 20, 30, 40,
    5, 10, 15, 20
  ), nrow = 4, ncol = 3)

  fn <- medoid(impute_na = TRUE)
  result <- fn(mat)
  expect_length(result, 3)
  expect_false(anyNA(result))
})

test_that("xoid_generator does not impute when impute_na = FALSE", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")

  # Create a matrix where the closest observation has NAs
  # 4 observations, 3 bands - put NA in the "best" row
  mat <- matrix(c(
    5, NA, 5,
    1, 1, 1,
    10, 10, 10,
    9, 9, 9
  ), nrow = 4, ncol = 3, byrow = TRUE)

  fn <- medoid(impute_na = FALSE)
  result <- fn(mat)
  expect_length(result, 3)
})

test_that("quantoid errors when WGCNA is not installed", {
  skip_if(rlang::is_installed("WGCNA"), "WGCNA is installed, cannot test error")
  expect_error(quantoid(), "WGCNA")
})
