close_it <- function(ds) {
  if (ds$isOpen()) {
    ds$close()
  }
}
split_into_bands <- function(x, n_groups) {
  split(x, ceiling(seq_along(x) / (length(x) / n_groups)))
}

test_that("chaining vrt functions works", {
  # TODO: these tests need a bit more but first we should sort proper masking.
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ex_collect <- vrt_collect(
    s2files
  )
  t_block <- ex_collect[[1]][[1]]

  ndvi_ex <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    ) |>
    vrt_warp(t_block$srs, t_block$bbox, t_block$res) |>
    vrt_derived_block(
      ndvi ~ (B08 - B04) / (B08 + B04),
      ndvitimes10 ~ ((B08 - B04) / (B08 + B04)) * 10
    ) |>
    vrt_set_nodata(-999)

  expect_true(ndvi_ex$n_items == 5)

  purrr::walk(ndvi_ex[[1]], \(x) {
    expect_s3_class(x, "vrt_block")
    expect_true("ndvi" %in% x$assets)
  })

  ndvi_ex_compute <- vrt_compute(
    ndvi_ex
  )

  # plot_raster_src(ndvi_ex_compute[1], 2)

  ds <- new(gdalraster::GDALRaster, ndvi_ex_compute[1])
  vals <- gdalraster::read_ds(ds)
  vals <- split_into_bands(vals, attr(vals, "gis")$dim[3])
  expect_equal(
    sum(vals[[1]], na.rm = TRUE) * 10,
    sum(vals[[2]], na.rm = TRUE),
    tolerance = 0.1
  )
  close_it(ds)

  add_pix_funs <- ndvi_ex |>
    vrt_set_gdal_pixelfun("sqrt") |>
    vrt_compute()

  ds2 <- new(gdalraster::GDALRaster, add_pix_funs[1])
  vals2 <- gdalraster::read_ds(ds2)
  vals2 <- split_into_bands(vals2, attr(vals2, "gis")$dim[3])
  expect_gt(sum(vals2[[1]], na.rm = TRUE), sum(vals[[1]], na.rm = TRUE))
  expect_lt(sum(vals2[[2]], na.rm = TRUE), sum(vals[[2]], na.rm = TRUE))
  close_it(ds2)
})
