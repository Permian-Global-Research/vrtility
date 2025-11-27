close_it <- function(ds) {
  if (ds$isOpen()) {
    ds$close()
  }
}

multiband_tests <- function() {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(
    s2files,
    datetimes = c(
      # these dates are made up btw.
      "2024-01-01",
      "2024-03-01",
      "2024-05-01",
      "2024-07-01",
      "2024-09-01"
    )
  )
  t_block <- ex_collect[[1]][[1]]

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    )
  ex_collect_mask_warp <- ex_collect_mask |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    )

  # Geomedian weizfeld:
  ex_geomed_weizfeld <- multiband_reduce(
    x = ex_collect_mask_warp,
    reduce_fun = geomedian(weizfeld = TRUE)
  )

  expect_true(all(file.exists(ex_geomed_weizfeld)))
  ds <- new(gdalraster::GDALRaster, ex_geomed_weizfeld)
  vals <- gdalraster::read_ds(ds)

  expect_equal(ds$getMetadataItem(0, "datetime", ""), "median date: 2024-05-01")
  expect_gt(sum(vals, na.rm = TRUE), 635000000)
  close_it(ds)

  # Geomedian weizfeld impute_na = FALSE:
  ex_geomed_weizfeld_na <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = geomedian(weizfeld = TRUE, impute_na = FALSE)
  )

  expect_true(all(file.exists(ex_geomed_weizfeld_na)))
  ds <- new(gdalraster::GDALRaster, ex_geomed_weizfeld_na)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 635000000)
  close_it(ds)

  # Geomedian Gmedian:
  ex_geomed_gmedian <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = geomedian(weizfeld = FALSE)
  )
  expect_true(all(file.exists(ex_geomed_gmedian)))
  ds <- new(gdalraster::GDALRaster, ex_geomed_gmedian)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 63270000)
  close_it(ds)

  # test if recollect works:
  # Geomedian Gmedian:
  ex_geomed_gmedian_coll <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = geomedian(weizfeld = FALSE),
    recollect = TRUE
  )

  expect_s3_class(
    ex_geomed_gmedian_coll,
    "vrt_block"
  )

  # medoid
  ex_medoid <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = medoid()
  )

  expect_true(all(file.exists(ex_medoid)))
  ds <- new(gdalraster::GDALRaster, ex_medoid)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 624500000)
  close_it(ds)

  # medoid impute_na = FALSE:
  ex_medoid_na <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = medoid(impute_na = FALSE)
  )

  expect_true(all(file.exists(ex_medoid_na)))
  ds <- new(gdalraster::GDALRaster, ex_medoid_na)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 624500000)
  close_it(ds)

  # quantoid
  ex_quantoid <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = quantoid(probability = 0.1)
  )
  expect_true(all(file.exists(ex_quantoid)))
  ds <- new(gdalraster::GDALRaster, ex_quantoid)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 591700000)
  close_it(ds)
  # quantoid impute_na = FALSE:
  ex_quantoid_na <- multiband_reduce(
    ex_collect_mask_warp,
    reduce_fun = quantoid("maximum", probability = 0.1, impute_na = FALSE)
  )
  expect_true(all(file.exists(ex_quantoid_na)))
  ds <- new(gdalraster::GDALRaster, ex_quantoid_na)
  vals <- gdalraster::read_ds(ds)
  expect_gt(sum(vals, na.rm = TRUE), 591700000)
  close_it(ds)
}

test_that("multiband_reduce works async", {
  # NOTE: we use expect_gt rather than matching exact values because windows
  # tests give different values - presumably some floadting point nonsence.

  if (!mirai::daemons_set()) {
    mirai::daemons(2)
  }

  multiband_tests()
})

test_that("multiband_reduce works synchronously", {
  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  multiband_tests()
})
