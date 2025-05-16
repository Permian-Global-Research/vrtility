test_that("assertions work", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ex_collect <- vrt_collect(s2files)
  test_xml <- xml2::read_xml(as.character(ex_collect[[1]][[1]]$vrt))
  band1 <- xml2::xml_find_first(test_xml, ".//.//VRTRasterBand")
  xml2::xml_add_child(band1, "SillyBilly", "wolly")
  tvrt <- fs::file_temp(ext = "vrt")
  xml2::write_xml(test_xml, tvrt)
  expect_warning(v_assert_valid_schema(tvrt))

  expect_error(v_assert_true(FALSE, "testy"))

  expect_equal(v_assert_res(c(2, 2)), c(2, 2))
  expect_equal(v_assert_res(2), c(2, 2))
  expect_error(v_assert_res(c(2, 2, 2)))

  # tests assert_srs_len - vrt_stack should fail with multple srs in collection
  expect_error(vrt_stack(ex_collect))

  # tests assert_files_exist - vrt_collect should fail with non-existent files
  expect_error(vrt_collect(c("notmyfile.tif", "northisone.tif")))
})
