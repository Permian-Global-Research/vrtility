test_that("multiplication works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  a <- c(vrt_collect(s2files), vrt_collect(s2files))
  expect_s3_class(a, "vrt_collection")
  expect_equal(a$n_items, 10)

  b <- c(vrt_collect(s2files), vrt_collect(s2files)[[1]][[1]])
  expect_s3_class(b, "vrt_collection")
  expect_equal(b$n_items, 6)

  d <- c(vrt_collect(s2files)[[1]][[1]], vrt_collect(s2files))
  expect_s3_class(d, "vrt_collection")
  expect_equal(d$n_items, 6)
})
