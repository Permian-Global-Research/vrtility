test_that("raster plotting works", {
  skip_on_os("windows")
  skip_on_ci()
  skip_on_covr()
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  ex_collect

  ex_vrt_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    )

  vdiffr::expect_doppelganger(
    "collect plot works",
    plot(ex_vrt_mask, item = 2, c(3, 2, 1))
  )

  t_block <- ex_vrt_mask[[1]][[1]]

  vdiffr::expect_doppelganger(
    "block plot works",
    plot(t_block, 2)
  )

  ex_vrt_mask_warp <- vrt_warp(
    ex_vrt_mask,
    t_srs = t_block$srs,
    te = t_block$bbox,
    tr = t_block$res
  )

  vdiffr::expect_doppelganger(
    "warp plot works",
    plot(ex_vrt_mask_warp, item = 3, c(3, 2, 1))
  )

  ex_vrt_mask_warp_stack <- vrt_stack(ex_vrt_mask_warp) |>
    vrt_set_py_pixelfun()

  vdiffr::expect_doppelganger(
    "stack plot works",
    plot(ex_vrt_mask_warp_stack, c(3, 2, 1))
  )
})
