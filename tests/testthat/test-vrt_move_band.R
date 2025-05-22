test_that("vrt_move_band works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ex_stack <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    )

  ex_band_move <- vrt_move_band(
    ex_stack,
    band_idx = 1,
    after = 2
  )

  expect_true(all(ex_band_move$assets == c("B03", "B02", "B04")))
})
