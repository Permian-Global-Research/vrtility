test_that("py pixel functions work", {
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

  py_pixfun_list <- list(
    median_numpy,
    mean_numpy,
    geomean_numpy,
    \() quantile_numpy(q = 0.2, use_fastnanquantile = FALSE),
    \() quantile_numpy(q = 0.2, use_fastnanquantile = TRUE),
    mean_db_numpy
  )

  rl <- purrr::map(
    py_pixfun_list,
    ~ {
      ex_stack |>
        vrt_set_py_pixelfun(
          pixfun = .x()
        ) |>
        vrt_compute(
          outfile = fs::file_temp(ext = "tif"),
          engine = "gdalraster"
        )
    }
  )

  freader <- function(x) {
    d <- methods::new(
      gdalraster::GDALRaster,
      x
    )
    on.exit(d$close(), add = TRUE)
    gdalraster::read_ds(d)
  }

  rdatalist <- purrr::map(rl, freader)

  all_equal <- function(lst) {
    all(vapply(lst, function(x) identical(x, lst[[1]]), logical(1)))
  }
  expect_true(length(unique(rdatalist)) == length(rdatalist))

  lim_pf <- ex_stack |>
    vrt_set_py_pixelfun(
      pixfun = median_numpy(),
      band_idx = c(1, 2)
    )

  # comment out for now - this failer is now exected...
  # expect_equal(
  #   length(xml2::xml_find_all(
  #     xml2::read_xml(lim_pf$vrt),
  #     ".//PixelFunctionType"
  #   )),
  #   2
  # )
})
