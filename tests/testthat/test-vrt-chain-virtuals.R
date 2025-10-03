test_that("chaining vrt functions works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ndvi <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    ) |>
    vrt_warp(t_block$srs, t_block$bbox, t_block$res) |>
    vrt_derived_block(ndvi ~ (B08 - B04) / (B08 + B04))

  purrr::walk(seq_len(ndvi$n_items), function(i) {
    plot(ndvi, i, na_col = "#ffffff00")
  })
  plot(ndvi)

  exc_pypf <- vrt_set_py_pixelfun(ex_collect) |>
    vrt_set_py_pixelfun()
  plot(exc_pypf, 1)
  exc_gdal <- vrt_set_gdal_pixelfun(ex_collect, "sqrt") |>
    vrt_set_gdal_pixelfun("sqrt")
  plot(exc_gdal)
  print(exc_gdal, pixfun = TRUE)
  print(exc_pypf, pixfun = TRUE)

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    )

  x <- vrt_set_gdal_pixelfun(ex_collect_mask, "pow", power = 2) |>
    # vrt_set_scale(scale = 1, offset = 0) |>
    vrt_compute(
      outfile = fs::file_temp(ext = "tif"),
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      add_cl_arg = c("-ot", "Float32")
    )

  plot_raster_src(x[1], c(3, 2, 1))

  expect_error()
})
