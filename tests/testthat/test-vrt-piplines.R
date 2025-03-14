test_that("full vrt pipeline works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]
  expect_snapshot(print(t_block))

  expect_equal(ex_collect$n_items, 5)
  expect_equal(length(ex_collect$srs), 3)

  expect_snapshot(print(ex_collect))
  expect_snapshot(print(ex_collect, blocks = TRUE))

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      valid_bits = c(4, 5, 6, 7, 11),
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
  expect_snapshot(print(ex_collect_mask_warp, maskfun = TRUE))

  coll_compute <- vrt_compute(
    ex_collect_mask_warp,
    outfile = fs::file_temp(ext = "tif")
  )

  expect_true(all(file.exists(coll_compute)))
  expect_true(all(file.size(coll_compute) > 0))

  ex_collect_mask_warp_stack <- vrt_stack(ex_collect_mask_warp)
  expect_s3_class(ex_collect_mask_warp_stack, "vrt_stack_warped")
  expect_snapshot(print(ex_collect_mask_warp_stack))

  ex_collect_mask_warp_stack_med <- vrt_set_pixelfun(ex_collect_mask_warp_stack)
  expect_s3_class(ex_collect_mask_warp_stack_med, "vrt_stack_warped")
  expect_snapshot(print(ex_collect_mask_warp_stack_med))
  expect_snapshot(print(ex_collect_mask_warp_stack_med, pixfun = TRUE))

  exe_comp <- vrt_compute(
    ex_collect_mask_warp_stack_med,
    outfile = fs::file_temp(ext = "tif"),
    engine = "translate"
  )

  expect_true(fs::file_size(exe_comp) > 0)

  exe_compwarp <- vrt_compute(
    ex_collect_mask_warp_stack_med,
    outfile = fs::file_temp(ext = "tif")
  )

  expect_true(fs::file_size(exe_compwarp) > 0)

  ds <- methods::new(gdalraster::GDALRaster, exe_compwarp)
  withr::defer(if (ds$isOpen()) ds$close())
  r <- gdalraster::read_ds(ds)
  expect_gt(sum(r, na.rm = TRUE), 6e+08)

  # numba mask no median
  testthat::skip_on_os("mac") # numba issues on mac https://github.com/numba/numba/issues/9812
  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      valid_bits = c(4, 5, 6, 7, 11),
      drop_mask_band = FALSE,
      mask_pixfun = bitmask_numba()
    ) |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    vrt_stack() |>
    vrt_set_pixelfun(pixfun = median_numba()) |>
    vrt_compute(outfile = fs::file_temp(ext = "tif"))

  expect_true(fs::file_size(ex_collect_mask) > 0)

  testthat::skip_on_os("windows")
  vdiffr::expect_doppelganger(
    "s2 exeter plots",
    plot_raster_src(exe_compwarp, c(3, 2, 1))
  )
})


test_that("vrt_collect works with rstac doc_items", {
  skip_if_offline()
  skip_on_cran()

  bbox <- gdalraster::bbox_from_wkt(
    wkt = wk::wkt("POINT (-3.51 50.72)"),
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
      "2024-08-26T11:21:11.024000Z",
      "2024-08-16T11:21:11.024000Z",
      "2024-08-01T11:21:19.024000Z",
      "2024-06-17T11:21:21.024000Z",
      "2024-06-02T11:21:19.024000Z"
    )
  )

  ex_collect_mask <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      valid_bits = c(4, 5, 6, 7, 11),
      drop_mask_band = TRUE
    )

  expect_false(is.null(ex_collect_mask$maskfun))
  expect_false(is.null(ex_collect_mask$mask_band_name))
  expect_s3_class(ex_collect_mask, "vrt_collection")

  expect_snapshot(print(ex_collect_mask))
  expect_snapshot(print(ex_collect_mask, blocks = TRUE, maskfun = TRUE))
})
