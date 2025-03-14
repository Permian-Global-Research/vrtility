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
  r <- gdalraster::read_ds(ds)
  expect_equal(sum(r, na.rm = TRUE), 635649405)

  vdiffr::expect_doppelganger(
    "s2 exeter plots",
    plot_raster_src(exe_compwarp, c(3, 2, 1))
  )
})
