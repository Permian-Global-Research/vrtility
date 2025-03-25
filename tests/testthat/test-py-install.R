test_that("python can be installed", {
  expect_invisible(
    vrtility_py_require(c("numpy"))
  )
})

test_that("python env can be located", {
  expect_true(fs::file_exists(Sys.getenv("VRTILITY_PY_EXECUTABLE")))
  expect_gte(as.numeric(Sys.getenv("VRTILITY_PY_VERSION_MAJOR")), 3)
  expect_gte(as.numeric(Sys.getenv("VRTILITY_PY_VERSION_MINOR")), 0)
})

test_that("python version parses correctly", {
  expect_type(as.numeric(Sys.getenv("VRTILITY_PY_VERSION_MINOR")), "double")
  expect_false(is.na(as.numeric(Sys.getenv("VRTILITY_PY_VERSION_MINOR"))))
})
