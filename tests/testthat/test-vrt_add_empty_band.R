test_that("vrt_add_empty_band works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ex_band_eb_ns <- vrt_add_empty_band(
    ex_collect,
    after = 1,
    description = "TESTBAND"
  )

  eb0 <- xml2::read_xml(ex_band_eb_ns[[1]][[1]]$vrt)
  tb0 <- xml2::xml_find_all(eb0, ".//.//Scale")
  expect_true(length(tb0) == 0)

  ex_band_eb <- vrt_add_empty_band(
    ex_collect,
    after = 1,
    description = "TESTBAND",
    scale_value = 1e-4
  )

  expect_true("TESTBAND" %in% ex_band_eb$assets)

  eb1 <- xml2::read_xml(ex_band_eb[[1]][[1]]$vrt)
  tb1 <- xml2::xml_find_all(eb1, ".//.//VRTRasterBand")[2]
  tbdesc <- xml2::xml_find_all(tb1, ".//Description") |>
    xml2::xml_text()
  scale_val <- xml2::xml_find_all(tb1, ".//Scale")[1] |>
    xml2::xml_text()

  expect_length(tb1, 1)
  expect_equal(as.numeric(scale_val), 1e-4)
  expect_equal(tbdesc, "TESTBAND")

  ex_band_eb2 <- vrt_add_empty_band(
    ex_band_eb,
    after = 1,
    description = "TESTBAND2"
  )
  eb2 <- xml2::read_xml(ex_band_eb2[[1]][[1]]$vrt)
  tb2 <- xml2::xml_find_all(eb2, ".//.//VRTRasterBand")[2]
  tbdesc2 <- xml2::xml_find_all(tb2, ".//Description") |>
    xml2::xml_text()
  scale_val2 <- xml2::xml_find_all(tb2, ".//Scale")[1] |>
    xml2::xml_text()

  expect_length(tb2, 1)
  expect_equal(as.numeric(scale_val2), 1e-4)
  expect_equal(tbdesc2, "TESTBAND2")
})
