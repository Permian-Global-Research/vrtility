test_that("vrt_derived_block works", {
  testthat::skip_if_not(check_muparser(), message = "muparser not available")

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files) |>
    vrt_set_scale(scale_value = 0.0001, offset_value = -0.1, band_idx = 1:4)
  t_block <- ex_collect[[1]][[1]]

  ex_ndvi <- vrt_derived_block(
    ex_collect,
    ndvi ~ (B08 - B04) / (B08 + B04)
  )

  testthat::expect_snapshot(print(ex_ndvi, pixfun = TRUE))

  ex_ndvifiles <- vrt_compute(
    ex_ndvi,
    t_srs = t_block$srs,
    te = t_block$bbox,
    tr = t_block$res,
    outfile = fs::file_temp(ext = ".tif"),
    engine = "warp"
  )

  expect_true(all(file.exists(ex_ndvifiles)))

  ds <- methods::new(gdalraster::GDALRaster, ex_ndvifiles[1])
  on.exit(ds$close())
  ds_mean <- mean(gdalraster::read_ds(ds), na.rm = TRUE)
  testthat::expect_gt(ds_mean, 0.1)
  testthat::expect_lt(ds_mean, 0.9)

  expect_identical(ds$getNoDataValue(1), NaN)

  zt <- vrt_derived_block(
    t_block,
    zero_test ~ B08 * 0
  ) |>
    vrt_compute()

  dszt <- methods::new(gdalraster::GDALRaster, zt)
  on.exit(dszt$close(), add = TRUE)
  unique_vals <- unique(gdalraster::read_ds(dszt))
  expect_true(all(unique_vals %in% c(0, NA) | is.na(unique_vals)))

  expect_identical(dszt$getNoDataValue(1), NaN)
})
