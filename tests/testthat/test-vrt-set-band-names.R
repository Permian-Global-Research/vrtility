test_that("vrt_set_band_names works", {
  expect_error(vrt_set_band_names(1, c("blue", "green", "red", "mask")))

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)
  t_block <- ex_collect[[1]][[1]]
  ex_c_rename <- vrt_set_band_names(
    ex_collect,
    c("blue", "green", "red", "mask")
  )

  expect_true(all(ex_c_rename$assets != ex_collect$assets))

  ex_stack <- vrt_warp(
    ex_c_rename,
    t_srs = t_block$srs,
    te = t_block$bbox,
    tr = t_block$res
  ) |>
    vrt_stack()

  ex_stack_rename <- vrt_set_band_names(
    ex_stack,
    c("bluey", "greeny", "redy", "masky")
  )

  expect_true(all(ex_stack_rename$assets != ex_stack$assets))
})
