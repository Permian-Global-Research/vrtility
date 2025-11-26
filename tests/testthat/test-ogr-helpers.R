test_that("ogr_bbox_from_file works", {
  f <- system.file("extdata/ynp_fires_1984_2022.gpkg", package = "gdalraster")
  bbox_native <- ogr_bbox_from_file(f, latlon = FALSE)
  expect_snapshot(bbox_native)
  bbox_latlon <- ogr_bbox_from_file(f, latlon = TRUE)
  expect_snapshot(bbox_latlon)
})

test_that("ogr_srs_from_file works", {
  f <- system.file("extdata/ynp_fires_1984_2022.gpkg", package = "gdalraster")
  srs_wkt <- ogr_srs_from_file(f)
  expect_snapshot(srs_wkt)
})
