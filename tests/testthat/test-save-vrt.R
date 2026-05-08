test_that("save_vrt works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ex_vrt <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    ) |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    vrt_stack() |>
    vrt_set_py_pixelfun()

  saved_file <- vrt_save(ex_vrt)
  ds1 <- methods::new(gdalraster::GDALRaster, saved_file)
  withr::defer(if (ds1$isOpen()) ds1$close())
  r1 <- compute_with_py_env({
    set_gdal_config(gdal_config_options())
    gdalraster::read_ds(ds1)
  })
  expect_type(r1, "integer")

  testthat::expect_s4_class(ds1, "Rcpp_GDALRaster")

  expect_error(saved_custom_file <- vrt_save(ex_vrt, "scrapit.tif"))
  withr::defer(
    if (fs::file_exists("scrapit.tif")) fs::file_delete("scrapit.tif")
  ) # shouldnt be needed.
  saved_custom_file <- vrt_save(ex_vrt, "scrapit.vrt")
  withr::defer(
    if (fs::file_exists("scrapit.vrt")) fs::file_delete("scrapit.vrt")
  )

  ds2 <- methods::new(gdalraster::GDALRaster, saved_custom_file)
  withr::defer(if (ds2$isOpen()) ds2$close())

  expect_s4_class(ds2, "Rcpp_GDALRaster")

  r2 <- compute_with_py_env({
    set_gdal_config(gdal_config_options())
    gdalraster::read_ds(ds2)
  })
  expect_type(r2, "integer")
})


read_vrt_values <- function(path) {
  ds <- methods::new(gdalraster::GDALRaster, path)
  on.exit(if (ds$isOpen()) ds$close())
  gdalraster::read_ds(ds)
}


test_that("vrt_save bundle mode produces a self-contained directory", {
  s2files <- fs::dir_ls(
    system.file("s2-data", package = "vrtility"),
    regexp = "exe_2024-0[78].*\\.tif$"
  )

  ex_vrt <- s2files |>
    vrt_collect() |>
    vrt_stack()

  ref_file <- vrt_save(ex_vrt)
  ref_vals <- read_vrt_values(ref_file)

  bundle_root <- fs::path(withr::local_tempdir(), "diff.vrt")
  saved <- vrt_save(ex_vrt, bundle_root, bundle = TRUE)

  expect_true(fs::file_exists(saved))
  expect_identical(
    as.character(fs::path_dir(saved)),
    as.character(fs::path_abs(fs::path_dir(bundle_root)))
  )

  bundle_files <- fs::dir_ls(fs::path_dir(saved))
  expect_gt(length(bundle_files), 1) # root VRT plus intermediates

  # every <SourceFilename> in the bundled VRTs that is not a URL must resolve
  # to a file inside the bundle (relativeToVRT = "1") or to an absolute path
  # that exists on disk (a non-bundled local raster).
  vrt_paths <- bundle_files[fs::path_ext(bundle_files) == "vrt"]
  purrr::walk(vrt_paths, function(p) {
    doc <- xml2::read_xml(p)
    purrr::walk(
      xml2::xml_find_all(doc, ".//SourceFilename"),
      function(node) {
        raw <- xml2::xml_text(node)
        if (assert_is_url(raw)) return(invisible())
        rel <- xml2::xml_attr(node, "relativeToVRT")
        if (!is.na(rel) && rel == "1") {
          expect_true(fs::file_exists(fs::path(fs::path_dir(p), raw)))
        } else {
          expect_true(fs::file_exists(raw))
        }
      }
    )
  })

  # bundle output must produce identical pixel values
  expect_equal(read_vrt_values(saved), ref_vals)
})


test_that("vrt_save include_rasters bundles local sources and is portable", {
  s2files <- fs::dir_ls(
    system.file("s2-data", package = "vrtility"),
    regexp = "exe_2024-0[78].*\\.tif$"
  )

  ex_vrt <- s2files |>
    vrt_collect() |>
    vrt_stack()

  ref_vals <- read_vrt_values(vrt_save(ex_vrt))

  bundle_dir <- withr::local_tempdir()
  bundle_root <- fs::path(bundle_dir, "diff.vrt")
  saved <- vrt_save(
    ex_vrt,
    bundle_root,
    bundle = TRUE,
    include_rasters = TRUE
  )

  # all source rasters should now live inside the bundle dir
  src_basenames <- fs::path_file(s2files)
  expect_true(all(
    fs::file_exists(fs::path(fs::path_dir(saved), src_basenames))
  ))

  # portability: wipe the cache, the bundle should still resolve
  cache_vrts <- fs::dir_ls(getOption("vrt.cache"), glob = "*.vrt")
  fs::file_delete(cache_vrts)
  expect_equal(read_vrt_values(saved), ref_vals)
})


test_that("vrt_save validates include_rasters / bundle combination", {
  s2files <- fs::dir_ls(
    system.file("s2-data", package = "vrtility"),
    regexp = "exe_2024-0[78].*\\.tif$"
  )
  ex_vrt <- s2files |>
    vrt_collect() |>
    vrt_stack()

  expect_error(
    vrt_save(
      ex_vrt,
      fs::path(withr::local_tempdir(), "x.vrt"),
      include_rasters = TRUE
    ),
    "include_rasters"
  )
})


test_that("vrt_save bundle warns once for remote sources with include_rasters", {
  fake_xml <- xml2::read_xml(
    paste0(
      "<VRTDataset rasterXSize=\"10\" rasterYSize=\"10\">",
      "<VRTRasterBand dataType=\"Byte\" band=\"1\">",
      "<SimpleSource>",
      "<SourceFilename relativeToVRT=\"0\">",
      "/vsicurl/https://example.com/foo.tif",
      "</SourceFilename>",
      "<SourceBand>1</SourceBand>",
      "</SimpleSource>",
      "</VRTRasterBand>",
      "</VRTDataset>"
    )
  )
  fake_block <- structure(
    list(vrt = as.character(fake_xml)),
    class = c("vrt_block", "list")
  )

  out <- fs::path(withr::local_tempdir(), "remote.vrt")
  expect_warning(
    vrt_save(fake_block, out, bundle = TRUE, include_rasters = TRUE),
    "remote source"
  )

  # no warning when the user has not opted into raster bundling
  out2 <- fs::path(withr::local_tempdir(), "remote2.vrt")
  expect_no_warning(
    vrt_save(fake_block, out2, bundle = TRUE, include_rasters = FALSE)
  )
})
