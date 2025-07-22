test_that("set_gdal_cache_max works", {
  gcm <- set_gdal_cache_max(0.05)
  expect_equal(
    as.numeric(gcm),
    0.05 * as.numeric(memuse::Sys.meminfo()$totalram)
  )
})
