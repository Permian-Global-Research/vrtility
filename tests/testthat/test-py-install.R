test_that("python can be installed", {
  expect_invisible(
    vrtility_py_require(c("numpy"))
  )
})

test_that("python env can be located", {
  expect_true(fs::file_exists(getOption("vrt.py_executable")))
  expect_gte(as.numeric(getOption("vrt.py_version_major")), 3)
  expect_gte(as.numeric(getOption("vrt.py_version_minor")), 0)
})

test_that("python version parses correctly", {
  expect_type(as.numeric(getOption("vrt.py_version_minor")), "double")
  expect_false(is.na(as.numeric(getOption("vrt.py_version_minor"))))
})
