sbm2m_tests <- function() {
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

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    )

  ex_collect_mask_warp <- ex_collect_mask |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    )

  hmpfiles <- ex_collect_mask_warp |>
    singleband_m2m(
      m2m_fun = hampel_filter(k = 1L, t0 = 0, impute_na = TRUE)
    )

  # check datetime metadata set correctly
  ds <- methods::new(
    gdalraster::GDALRaster,
    hmpfiles[[1]]
  )
  on.exit(ds$close(), add = TRUE)
  testthat::expect_equal(ds$getMetadataItem(0, "datetime", ""), "2024-01-01")

  expect_true(all(fs::file_exists(hmpfiles)))

  ras_vals <- function(x) {
    d <- methods::new(
      gdalraster::GDALRaster,
      x
    )
    on.exit(d$close(), add = TRUE)
    rd <- gdalraster::read_ds(d)
    scv <- d$getScale(1)
    if (!is.na(scv)) {
      rd <- rd * scv
    }
    rd
  }

  expect_false(identical(
    ras_vals(hmpfiles[[2]]),
    ras_vals(s2files[[2]])
  ))

  hmpfilesrecoll <- singleband_m2m(
    ex_collect_mask_warp,
    m2m_fun = hampel_filter(k = 1L, t0 = 0, impute_na = TRUE),
    recollect = TRUE
  )

  testthat::expect_s3_class(hmpfilesrecoll, "vrt_collection_warped")

  # check vrt_set_scale works
  hmpfiles_scaled <- ex_collect_mask_warp |>
    vrt_set_scale(0.0001) |>
    singleband_m2m(
      m2m_fun = hampel_filter(k = 1L, t0 = 0, impute_na = TRUE)
    )

  expect_equal(
    mean(ras_vals(hmpfiles[1]) / ras_vals(hmpfiles_scaled[1]), na.rm = TRUE),
    10000,
    tolerance = 0.1
  )
}


test_that("singleband_m2m works async", {
  if (!mirai::daemons_set()) {
    mirai::daemons(2)
  }

  sbm2m_tests()
})

test_that("singleband_m2m works synchronously", {
  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  sbm2m_tests()
})
