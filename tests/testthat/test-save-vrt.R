test_that("save_vrt works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ex_vrt <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    ) |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    vrt_stack() |>
    vrt_set_pixelfun()

  saved_file <- vrt_save(ex_vrt)
  ds1 <- methods::new(gdalraster::GDALRaster, saved_file)
  withr::defer(if (ds1$isOpen()) ds1$close())
  r1 <- compute_with_py_env({
    set_gdal_config(gdal_config_opts())
    gdalraster::read_ds(ds1)
  })
  expect_type(r1, "integer")

  testthat::expect_s4_class(ds1, "Rcpp_GDALRaster")

  expect_error(saved_custom_file <- vrt_save(ex_vrt, "scrapit.tif"))
  withr::defer(
    if (fs::file_exists("scrapit.tif")) fs::file_delete("scrapit.tif")
  ) # shouldnt be needed.
  saved_custom_file <- vrt_save(ex_vrt, "scrapit.vrt")
  withr::defer(
    if (fs::file_exists("scrapit.vrt")) fs::file_delete("scrapit.vrt")
  )

  ds2 <- methods::new(gdalraster::GDALRaster, saved_custom_file)
  withr::defer(if (ds2$isOpen()) ds2$close())

  expect_s4_class(ds2, "Rcpp_GDALRaster")

  r2 <- compute_with_py_env({
    set_gdal_config(gdal_config_opts())
    gdalraster::read_ds(ds2)
  })
  expect_type(r2, "integer")
})
