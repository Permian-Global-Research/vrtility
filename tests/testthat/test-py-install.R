test_that("python can be installed", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  expect_invisible(
    vrtility_py_require(c("numpy"))
  )
})

test_that("python env can be located", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  expect_true(fs::file_exists(getOption("vrt.py_executable")))
  expect_gte(as.numeric(getOption("vrt.py_version_major")), 3)
  expect_gte(as.numeric(getOption("vrt.py_version_minor")), 0)
})

test_that("python version parses correctly", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  expect_type(as.numeric(getOption("vrt.py_version_minor")), "double")
  expect_false(is.na(as.numeric(getOption("vrt.py_version_minor"))))
})
