pipeline_tests <- function(v) {
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
  expect_snapshot(print(t_block))

  expect_equal(ex_collect$n_items, 5)
  expect_equal(length(ex_collect$srs), 3)

  expect_snapshot(print(ex_collect))
  expect_snapshot(print(ex_collect, blocks = TRUE))

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = FALSE
    )

  expect_false(is.null(ex_collect_mask$mask_band_name))
  expect_false(is.null(ex_collect_mask$maskfun))
  expect_s3_class(ex_collect_mask, "vrt_collection")

  ex_collect_mask_warp <- ex_collect_mask |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    )

  expect_equal(ex_collect_mask_warp$bbox, t_block$bbox)
  expect_equal(ex_collect_mask_warp$res, t_block$res)
  expect_equal(ex_collect_mask_warp$srs, t_block$srs)
  expect_s3_class(ex_collect_mask_warp, "vrt_collection_warped")
  expect_snapshot(print(ex_collect_mask_warp))
  expect_snapshot(print(ex_collect_mask_warp, xml = TRUE, maskfun = TRUE))

  coll_compute <- vrt_compute(
    ex_collect_mask_warp,
    outfile = fs::file_temp(ext = "tif")
  )

  expect_true(all(file.exists(coll_compute)))
  expect_true(all(file.size(coll_compute) > 0))

  ex_collect_mask_warp_stack <- vrt_stack(ex_collect_mask_warp)
  expect_s3_class(ex_collect_mask_warp_stack, "vrt_stack_warped")
  expect_snapshot(print(ex_collect_mask_warp_stack))

  ex_collect_mask_warp_stack_med <- vrt_set_py_pixelfun(
    ex_collect_mask_warp_stack
  )
  expect_s3_class(ex_collect_mask_warp_stack_med, "vrt_stack_warped")
  expect_snapshot(print(ex_collect_mask_warp_stack_med))
  expect_snapshot(print(ex_collect_mask_warp_stack_med, pixfun = TRUE))

  exe_comp <- vrt_compute(
    ex_collect_mask_warp_stack_med,
    engine = "translate",
    creation_options = gdal_creation_options(
      output_format = "COG"
    )
  )

  ds <- methods::new(gdalraster::GDALRaster, exe_comp)
  ds$getMetadata(0, "")

  expect_true(grepl("COG", ds$infoAsJSON()))
  expect_true(
    "median date: 2024-05-01" %in% ds$getMetadataItem(0, "datetime", "")
  )
  ds$close()

  expect_true(fs::file_size(exe_comp) > 0)

  exe_compwarp <- vrt_compute(
    ex_collect_mask_warp_stack_med,
    outfile = fs::file_temp(ext = "tif"),
    creation_options = gdal_creation_options(
      output_format = "COG"
    ),
    engine = "gdalraster"
  )

  gdalraster::dump_open_datasets()

  ds <- methods::new(gdalraster::GDALRaster, exe_compwarp)
  ds$getMetadata(0, "")
  expect_true(grepl("COG", ds$infoAsJSON()))
  expect_equal(ds$getMetadataItem(0, "datetime", ""), "median date: 2024-05-01")
  ds$close()

  expect_true(fs::file_size(exe_compwarp) > 0)

  ds <- methods::new(gdalraster::GDALRaster, exe_compwarp)

  r <- gdalraster::read_ds(ds)
  ds$close()

  expect_gt(sum(r, na.rm = TRUE), 6e+08)

  ex_collect_mask_gdr <- vrt_compute(
    ex_collect_mask_warp_stack_med,
    outfile = fs::file_temp(ext = "tif"),
    engine = "gdalraster",
    nsplits = 2L
  )

  expect_true(fs::file_size(ex_collect_mask_gdr) > 0)

  skip_on_os("windows")
  skip_on_ci()
  skip_on_covr()
  vdiffr::expect_doppelganger(
    glue::glue("{v} s2 exeter plots"),
    plot_raster_src(exe_compwarp, c(3, 2, 1))
  )
}


test_that("full vrt pipeline works async", {
  if (!mirai::daemons_set()) {
    mirai::daemons(2)
  }
  pipeline_tests(v = "async")
})

test_that("full vrt pipeline works synchronously", {
  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }
  pipeline_tests(v = "sync")
})


test_that("vrt_collect works with rstac doc_items", {
  skip_if_offline()
  skip_on_cran()

  bbox <- gdalraster::bbox_from_wkt(
    wkt = "POINT (-3.51 50.72)",
    extend_x = 0.05,
    extend_y = 0.03
  )

  te <- bbox_to_projected(bbox)
  trs <- attr(te, "wkt")

  s2_stac <- sentinel2_stac_query(
    bbox = bbox,
    start_date = "2024-06-01",
    end_date = "2024-08-30",
    max_cloud_cover = 50,
    assets = c("B02", "B03", "B04", "SCL")
  )
  # number of items:
  expect_equal(length(s2_stac$features), 5)
  expect_type(s2_stac, "list")
  expect_s3_class(s2_stac, "doc_items")

  ex_collect <- vrt_collect(s2_stac)

  expect_equal(ex_collect$n_items, 5)
  expect_equal(ex_collect$bbox, c(399960, 5590200, 509760, 5700000))
  expect_equal(
    ex_collect$date_time,
    c(
      "2024-06-02T11:21:19.024000Z",
      "2024-06-17T11:21:21.024000Z",
      "2024-08-01T11:21:19.024000Z",
      "2024-08-16T11:21:11.024000Z",
      "2024-08-26T11:21:11.024000Z"
    )
  )

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    )

  expect_false(is.null(ex_collect_mask$maskfun))
  expect_false(is.null(ex_collect_mask$mask_band_name))
  expect_s3_class(ex_collect_mask, "vrt_collection")

  expect_snapshot(print(ex_collect_mask))
  expect_snapshot(print(ex_collect_mask, blocks = TRUE, maskfun = TRUE))
})

test_that("pipeline extras", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)
  exc_pypf <- vrt_set_py_pixelfun(ex_collect)
  expect_true(length(purrr::map(exc_pypf$vrt, ~ .x$pixfun)) == exc_pypf$n_items)

  one_srs_collect <- vrt_collect(s2files[4:5])

  expect_error(vrt_stack(vrt_collect(s2files[1:3])))

  one_srs_collect_stack <- vrt_stack(one_srs_collect)
  one_srs_collect_stack_pf <- vrt_set_py_pixelfun(
    one_srs_collect_stack,
    pixfun = median_numpy()
  )
  expect_false(is.null(one_srs_collect_stack_pf$pixfun))

  one_srs_collect_stack_pf_mask <- vrt_set_maskfun(
    one_srs_collect_stack_pf,
    mask_band = "FAKEMASK",
    mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
  ) |>
    expect_error()

  expect_error(
    vrt_set_maskfun(
      "won't work",
      mask_band = "DOESNTEXIST",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11)
    )
  )
})


test_that("user no data passes to gdalraster vrt_compute method", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  v <- ex_collect <- vrt_collect(s2files[1])

  vw <- v |>
    vrt_warp(
      t_srs = v$srs,
      te = v$bbox,
      tr = v$res
    )

  y <- vrt_compute(
    vw,
    outfile = fs::file_temp(ext = "tif"),
    engine = "gdalraster",
    dst_nodata = -999
  )

  ds <- methods::new(gdalraster::GDALRaster, y)

  expect_equal(ds$getNoDataValue(band = 1), -999)
})
