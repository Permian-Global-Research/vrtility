test_that("is_temp_dir detects tempdir correctly", {
  expect_true(vrtility:::is_temp_dir(tempdir()))
  expect_true(vrtility:::is_temp_dir(file.path(tempdir(), "subdir")))
  expect_false(vrtility:::is_temp_dir("/some/other/path"))
})

test_that("cache_init_checks sets default option", {
  withr::with_options(list(vrt.cache = NULL), {
    vrtility:::cache_init_checks()
    expect_equal(getOption("vrt.cache"), tempdir())
  })
})

test_that("vrt_cache_set works", {
  tdir <- fs::file_temp("cache_test")
  on.exit(fs::dir_delete(tdir), add = TRUE)

  withr::with_options(list(vrt.cache = tempdir()), {
    # Setting to a new directory
    expect_message(
      result <- vrt_cache_set(tdir),
      "changed"
    )
    expect_equal(result, tdir)
    expect_equal(getOption("vrt.cache"), tdir)

    # Setting to the same directory
    expect_message(
      vrt_cache_set(tdir),
      "already set"
    )
  })
})

test_that("vrt_cache_destroy warns on tempdir", {
  withr::with_options(list(vrt.cache = tempdir()), {
    expect_warning(
      vrt_cache_destroy(),
      "tempdir"
    )
  })
})

test_that("vrt_cache_destroy works non-interactively", {
  # Must be outside tempdir() to avoid the is_temp_dir guard
  tdir <- file.path(tools::R_user_dir("vrtility", "cache"), "test_destroy")
  fs::dir_create(tdir)
  on.exit(try(fs::dir_delete(tdir), silent = TRUE), add = TRUE)

  withr::with_options(list(vrt.cache = tdir), {
    withCallingHandlers(
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage"),
      vrt_cache_destroy()
    )
    expect_false(fs::dir_exists(tdir))
  })
})
