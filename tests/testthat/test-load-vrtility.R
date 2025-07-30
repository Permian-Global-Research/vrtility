test_that("load vrtility works", {
  test_library <- function(package) {
    library(package = package, character.only = TRUE)
  }
  testthat::expect_no_error(
    test_library("vrtility")
  )
})
