test_that("call_gdal_translate works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  outfile <- fs::file_temp(ext = ".tif")

  result <- vrtility:::call_gdal_tanslate(
    s2files[1],
    outfile,
    cl_arg = c("-b", "1"),
    config_options = NULL
  )

  expect_equal(result, outfile)
  expect_true(fs::file_exists(outfile))
})

test_that("call_gdal_translate rejects missing file", {
  expect_error(
    vrtility:::call_gdal_tanslate(
      "does_not_exist.tif",
      fs::file_temp(ext = ".tif"),
      cl_arg = NULL,
      config_options = NULL
    )
  )
})
