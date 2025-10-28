test_that("vrt_set_nodata works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  ex_nodata <- vrt_set_nodata(
    ex_collect,
    nodatavalue = 9999,
    nodata = 100,
    band_idx = 1
  )

  ds <- methods::new(
    gdalraster::GDALRaster,
    ex_nodata[[1]][[1]]$vrt_src
  )
  on.exit(ds$close())
  expect_equal(ds$getNoDataValue(band = 1), 9999)
  expect_equal(ds$getNoDataValue(band = 2), 0)

  # test that source nodata values are written into VRT correctly
  xmldat <- xml2::read_xml(ex_nodata[[1]][[1]]$vrt)
  xml2::xml_find_all(
    xmldat,
    ".//NODATA"
  ) |>
    xml2::xml_text() |>
    as.numeric() |>
    expect_equal(c(100, 0, 0, 0, 0))
})
