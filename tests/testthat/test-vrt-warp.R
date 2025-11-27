vrt_warp_tests <- function() {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(
    s2files,
    datetimes = c(
      # these dates are made up btw.
      "2024-01-01",
      "2024-03-01",
      "2024-05-01",
      "2024-07-01",
      "2024-09-01"
    )
  )

  t_block <- ex_collect[[1]][[1]]

  ex_collect_warp <- ex_collect |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      quiet = FALSE,
      lazy = TRUE
    )

  ds <- methods::new(
    gdalraster::GDALRaster,
    ex_collect_warp$vrt[[1]]$vrt_src
  )
  expect_true(all(fs::path_ext(ds$getFileList()) == "vrt"))
  expect_true(all(ds$getBlockSize(1) == c(361, 128)))

  ds$close()

  ex_collect_warp <- ex_collect |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      quiet = FALSE,
      lazy = FALSE
    )

  ds <- methods::new(
    gdalraster::GDALRaster,
    ex_collect_warp$vrt[[1]]$vrt_src
  )
  expect_true(all(
    fs::path_ext(ds$getFileList()[2:length(ds$getFileList())]) == "tif"
  ))

  ds$close()

  ex_block_warp <- ex_collect[[1]][[2]] |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      quiet = FALSE,
      lazy = FALSE
    )

  ds <- methods::new(
    gdalraster::GDALRaster,
    ex_block_warp$vrt_src
  )
  expect_true(all(
    fs::path_ext(ds$getFileList()[2:length(ds$getFileList())]) == "tif"
  ))

  ds$close()

  ex_block_warp <- ex_collect[[1]][[2]] |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      quiet = FALSE,
      lazy = TRUE
    )

  ds <- methods::new(
    gdalraster::GDALRaster,
    ex_block_warp$vrt_src
  )
  expect_true(all(
    fs::path_ext(ds$getFileList()) == "vrt"
  ))

  ds$close()
}

test_that("vrt_warp works async", {
  if (!mirai::daemons_set()) {
    mirai::daemons(2)
  }
  vrt_warp_tests()
})

test_that("vrt_warp works synchronously", {
  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }
  vrt_warp_tests()
})
