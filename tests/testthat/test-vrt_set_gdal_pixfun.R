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
    pixfun = "interpolate_linear",
    t0 = 0,
    dt = 1,
    t = 8
  )
  # print(gpf, xml = TRUE)

  of <- vrt_compute(
    gpf,
    outfile = fs::file_temp(ext = "tif"),
    engine = "gdalraster"
  )

  plot_raster_src(
    of,
    c(3, 2, 1)
  )
})
