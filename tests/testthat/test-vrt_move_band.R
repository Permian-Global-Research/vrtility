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

  expect_true(all(ex_band_move$assets == c("B03", "B02", "B04", "B08")))
})

test_that("vrt_move_band default method errors", {
  expect_error(
    vrt_move_band("not_a_vrt", band_idx = 1, after = 2),
    class = "vrtility_type_error"
  )
})

test_that("vrt_move_band validates after and band_idx", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ex_collect <- vrt_collect(s2files)
  block <- ex_collect[[1]][[1]]

  # after out of range
  expect_error(
    vrt_move_band(block, band_idx = 1, after = -1),
    class = "vrtility_after_error"
  )
  expect_error(
    vrt_move_band(block, band_idx = 1, after = length(block$assets) + 2),
    class = "vrtility_after_error"
  )

  # band_idx out of range
  expect_error(
    vrt_move_band(block, band_idx = 0, after = 1),
    class = "vrtility_band_error"
  )
  expect_error(
    vrt_move_band(block, band_idx = length(block$assets) + 1, after = 1),
    class = "vrtility_band_error"
  )

  # band_idx == after
  expect_error(
    vrt_move_band(block, band_idx = 2, after = 2),
    class = "vrtility_band_error"
  )
})

test_that("vrt_move_band with after = 0 moves band to first position", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ex_collect <- vrt_collect(s2files)
  block <- ex_collect[[1]][[1]]

  original_assets <- block$assets
  moved <- vrt_move_band(block, band_idx = 3, after = 0)
  expect_equal(moved$assets[1], original_assets[3])
  expect_equal(moved$assets[2], original_assets[1])
})
