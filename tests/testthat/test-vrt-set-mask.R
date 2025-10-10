read_vals <- function(r) {
  compute_with_py_env(
    {
      ds <- methods::new(gdalraster::GDALRaster, r)
      on.exit(ds$close())
      gdalraster::read_ds(ds)
    }
  )
}
test_that("vrt_set_maskfun works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files[3])

  ex_collect_mask_0buff <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE,
      set_mask_pixfun = set_mask_numpy(buffer_size = 0)
    )

  ex_collect_mask_10buff <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE,
      set_mask_pixfun = set_mask_numpy(buffer_size = 3)
    )

  # test that varying masks and buffers change the output
  ex_nm <- sum(read_vals(ex_collect$vrt_src), na.rm = TRUE)
  ex_0b <- sum(read_vals(ex_collect_mask_0buff$vrt_src), na.rm = TRUE)
  ex_10b <- sum(read_vals(ex_collect_mask_10buff$vrt_src), na.rm = TRUE)

  expect_gt(ex_nm, ex_0b)
  expect_gt(ex_0b, ex_10b)

  expect_error(set_mask_numpy(buffer_size = -1))
})
