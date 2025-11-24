test_that("optimise_tiling functionality works", {
  t1 <- optimise_tiling(
    xs = 10000,
    ys = 10000,
    blksize = c(512, 512),
    nsplits = 1000
  )
  expect_equal(nrow(t1), 400)

  t2 <- optimise_tiling(
    xs = 10000,
    ys = 10000,
    blksize = c(285, 512),
    nsplits = 20
  )

  expect_snapshot(t2)
})
