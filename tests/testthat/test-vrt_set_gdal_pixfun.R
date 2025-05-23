test_that("vrt_set_gdal_pixfun works", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files)

  t_block <- ex_collect[[1]][[1]]

  ex_stack <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    ) |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    vrt_stack()

  exstack_fill <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      drop_mask_band = TRUE
    ) |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    singleband_m2m(m2m_fun = hampel_filter(impute_na = TRUE)) |>
    vrt_collect() |>
    vrt_warp(
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res
    ) |>
    vrt_stack()

  gpf <- vrt_set_gdal_pixfun(
    exstack_fill,
    pixfun = "min"
  )

  of <- vrt_compute(
    gpf,
    outfile = fs::file_temp(ext = "tif"),
    engine = "gdalraster"
  )

  singleband <- vrt_collect(of) |>
    vrt_set_scale(0.001) |>
    vrt_set_gdal_pixfun(
      pixfun = "scale"
    ) |>
    vrt_compute(
      outfile = fs::file_temp(ext = "tif"),
      t_srs = t_block$srs,
      te = t_block$bbox,
      tr = t_block$res,
      engine = "warp",
      apply_scale = TRUE
    )

  freader <- function(x) {
    d <- methods::new(
      gdalraster::GDALRaster,
      x
    )
    on.exit(d$close(), add = TRUE)
    gdalraster::read_ds(d)
  }

  expect_lt(
    sum(freader(singleband), na.rm = TRUE),
    sum(freader(of), na.rm = TRUE)
  )

  int_lin <- function(dt) {
    vrt_set_gdal_pixfun(
      exstack_fill,
      pixfun = "interpolate_linear",
      t0 = 0,
      dt = dt,
      t = 7
    ) |>
      vrt_compute(
        outfile = fs::file_temp(ext = "tif"),
        engine = "gdalraster"
      )
  }

  expect_false(identical(freader(int_lin(0.5)), freader(int_lin(2))))
})
