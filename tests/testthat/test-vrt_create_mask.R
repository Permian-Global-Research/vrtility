test_that("vrt_create_mask works", {
  # for some reason matplotlib build fails on GHA on macOS
  testthat::skip_on_os("mac")
  skip_if_not(
    reticulate::py_available(initialize = TRUE),
    "Python not available"
  )

  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files[4:5])

  exc_mask <- vrt_create_mask(
    ex_collect,
    c(red = 3, green = 2, nir = 4),
    maskfun = create_omnicloudmask()
  )
  expect_true("omnicloudmask" %in% exc_mask$assets)

  exc_mask_compute <- vrt_compute(
    exc_mask,
    fs::file_temp(ext = ".tif")
  )

  expect_length(exc_mask_compute, 2)
  expect_true(all(fs::file_exists(exc_mask_compute)))

  omc_check <- methods::new(gdalraster::GDALRaster, exc_mask_compute[1])

  vals <- gdalraster::read_ds(omc_check, bands = 6)
  expect_true(all(c(NA, 1, 2, 3) %in% unique(vals)))
})
