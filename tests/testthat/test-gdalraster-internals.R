test_that(".gr_flip_vertical reverses row order", {
  # 3 columns (xsize), 2 rows (ysize), 1 band
  # row0 = [1,2,3], row1 = [4,5,6]
  v <- c(1, 2, 3, 4, 5, 6)
  result <- .gr_flip_vertical(v, xsize = 3, ysize = 2, nbands = 1)
  expect_equal(result, c(4, 5, 6, 1, 2, 3))
})

test_that(".gr_flip_vertical works with multiple bands", {
  # 2 columns, 2 rows, 2 bands (band-sequential)
  v <- c(1, 2, 3, 4, 10, 20, 30, 40)
  result <- .gr_flip_vertical(v, xsize = 2, ysize = 2, nbands = 2)
  expect_equal(result, c(3, 4, 1, 2, 30, 40, 10, 20))
})
