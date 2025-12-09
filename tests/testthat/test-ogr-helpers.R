test_that("ogr_bbox_from_file works", {
  f <- system.file("extdata/ynp_fires_1984_2022.gpkg", package = "gdalraster")
  bbox_native <- ogr_bbox_from_file(f, latlon = FALSE)
  expect_snapshot(bbox_native)
  bbox_latlon <- ogr_bbox_from_file(f, latlon = TRUE)
  expect_snapshot(bbox_latlon)
  bbox_extended <- ogr_bbox_from_file(
    f,
    latlon = TRUE,
    extend_x = 0.1,
    extend_y = 0.2
  )
  expect_snapshot(bbox_extended)

  expect_identical(bbox_extended[1] - bbox_latlon[1], -0.1, tolerance = 1e-6)
  expect_identical(bbox_extended[2] - bbox_latlon[2], -0.2, tolerance = 1e-6)
})

test_that("ogr_srs_from_file works", {
  f <- system.file("extdata/ynp_fires_1984_2022.gpkg", package = "gdalraster")
  srs_wkt <- ogr_srs_from_file(f)
  expect_snapshot(srs_wkt)
})
