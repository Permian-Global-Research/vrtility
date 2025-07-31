test_that("load vrtility works", {
  expect_no_error(
    callr::r(function() loadNamespace("vrtility"))
  )
})
