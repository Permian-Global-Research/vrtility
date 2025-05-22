test_that("vrt_set_scale works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  ex_sc1 <- vrt_set_scale(
    ex_collect,
    scale_value = 1e-4
  )

  sc1 <- xml2::read_xml(ex_sc1[[1]][[1]]$vrt)
  bsc1 <- xml2::xml_find_all(sc1, ".//.//VRTRasterBand")

  scale_vals <- xml2::xml_find_all(bsc1, ".//Scale") |>
    xml2::xml_text()

  expect_true(length(scale_vals) == length(ex_sc1[[1]][[1]]$assets))
  expect_true(all(as.numeric(scale_vals) == 1e-4))

  ex_sc2 <- vrt_set_scale(
    ex_collect,
    scale_value = c(4, 3, 2, 1)
  )

  sc2 <- xml2::read_xml(ex_sc2[[1]][[1]]$vrt)
  bsc2 <- xml2::xml_find_all(sc2, ".//.//VRTRasterBand")
  scale_vals2 <- xml2::xml_find_all(bsc2, ".//Scale") |>
    xml2::xml_text()
  expect_true(length(scale_vals2) == length(ex_sc2[[1]][[1]]$assets))

  expect_true(all(as.numeric(scale_vals2) == c(4, 3, 2, 1)))

  expect_error(
    vrt_set_scale(
      ex_collect,
      scale_value = c(4, 3, 2)
    )
  )
})
