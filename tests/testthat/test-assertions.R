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

  expect_error(vrt_collect(c(s2files, "bigbadkenge.tif")))

  expect_length(
    assert_files_exist(
      c(s2files, "https://not-a-rea-url.tif"),
      url_possible = TRUE
    ),
    6
  )

  expect_error(
    assert_files_exist(
      c(s2files, "https://not-a-rea-url.tif"),
      url_possible = FALSE
    )
  )

  expect_error(
    v_assert_within_range(c(1, 2, 3), "test", 0, 2)
  )

  expect_error(
    v_assert_within_range(c(1, 2, 3), "test", 2, 4)
  )
})

test_that("v_assert_length_gt works", {
  expect_error(
    v_assert_length_gt(character(0), "x", 0, nullok = FALSE),
    class = "vrtility_length_error"
  )
  expect_error(
    v_assert_length_gt("a", "x", 1, nullok = FALSE),
    class = "vrtility_length_error"
  )
  expect_null(v_assert_length_gt(c("a", "b"), "x", 1))
  expect_null(v_assert_length_gt(NULL, "x", 1, nullok = TRUE))
})

test_that("v_assert_is_named works", {
  expect_error(
    v_assert_is_named(c(1, 2, 3), "x"),
    class = "vrtility_named_error"
  )
  expect_invisible(v_assert_is_named(c(a = 1, b = 2), "x"))
})

test_that("v_assert_hls_catalog works", {
  # Planetary Computer source
  expect_error(
    v_assert_hls_catalog(
      "https://planetarycomputer.microsoft.com/api/stac/v1/",
      "wrong"
    )
  )
  expect_silent(
    v_assert_hls_catalog(
      "https://planetarycomputer.microsoft.com/api/stac/v1/",
      "hls2-s30"
    )
  )

  # NASA EARTHDATA source
  expect_error(
    v_assert_hls_catalog(
      "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
      "wrong"
    )
  )
  expect_silent(
    v_assert_hls_catalog(
      "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
      "HLSS30_2.0"
    )
  )
})

test_that("v_assert_create_mask_fun_attrs works", {
  bad_fn <- function(x) x
  expect_error(
    v_assert_create_mask_fun_attrs(bad_fn, "maskfun"),
    class = "vrtility_maskcreator_fun_error"
  )

  good_fn <- function(x) x
  attr(good_fn, "mask_name") <- "test"
  attr(good_fn, "required_bands") <- c("red", "nir")
  attr(good_fn, "mask_description") <- "a test mask"
  expect_invisible(
    v_assert_create_mask_fun_attrs(good_fn, "maskfun")
  )
})

test_that("v_assert_mask_names_match works", {
  maskfun <- function(x) x
  attr(maskfun, "mask_name") <- "test"
  attr(maskfun, "required_bands") <- c("red", "nir")

  expect_error(
    v_assert_mask_names_match(c(blue = 1, green = 2), maskfun),
    class = "vrtility_maskfun_error"
  )
  expect_silent(
    v_assert_mask_names_match(c(red = 1, nir = 2), maskfun)
  )
})

test_that("v_assert_blosc works", {
  # blosc may or may not be available - just check it doesn't error unexpectedly
  if (check_blosc()) {
    expect_silent(v_assert_blosc("warn"))
  } else {
    expect_warning(v_assert_blosc("warn"))
    expect_error(v_assert_blosc("abort"))
  }
})

test_that("muparser_mask_warn produces correct warning", {
  expect_warning(
    muparser_mask_warn("test_func"),
    class = "muparser_not_available_warning"
  )
})
