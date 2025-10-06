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

  pixfunstep1 <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    ) |>
    vrt_warp(t_block$srs, t_block$bbox, t_block$res) |>
    vrt_set_gdal_pixelfun("sqrt")

  expect_true(pixfunstep1$n_items == 5)

  pixfunstep1_compute <- vrt_compute(
    pixfunstep1
  )

  t_block_compute <- vrt_compute(
    t_block
  )

  # plot_raster_src(ndvi_ex_compute[1], 2)

  ds <- new(gdalraster::GDALRaster, pixfunstep1_compute[1])
  vals <- gdalraster::read_ds(ds)
  vals <- split_into_bands(vals, attr(vals, "gis")$dim[3])
  tbds <- new(gdalraster::GDALRaster, t_block_compute)
  t_vals <- gdalraster::read_ds(tbds)
  t_vals <- split_into_bands(t_vals, attr(t_vals, "gis")$dim[3])

  expect_lt(
    sum(vals[[1]], na.rm = TRUE),
    sum(t_vals[[1]], na.rm = TRUE)
  )
  close_it(ds)
  close_it(tbds)

  testthat::skip_if_not(check_muparser(), message = "muparser not available")

  add_pix_funs <- ndvi_ex |>
    vrt_derived_block(
      t1 ~ B04 * 100,
      t2 ~ B04 * 1000,
    ) |>
    vrt_compute()

  ds2 <- new(gdalraster::GDALRaster, add_pix_funs[1])
  vals2 <- gdalraster::read_ds(ds2)
  vals2 <- split_into_bands(vals2, attr(vals2, "gis")$dim[3])
  expect_lt(sum(vals2[[1]], na.rm = TRUE), sum(vals2[[2]], na.rm = TRUE))
  close_it(ds2)
})
