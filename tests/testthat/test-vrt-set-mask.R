read_vals <- function(r) {
  compute_with_py_env({
    ds <- methods::new(gdalraster::GDALRaster, r)
    on.exit(ds$close())
    gdalraster::read_ds(ds)
  })
}

bitmask_test_vals <- function(use_muparser) {
  withr::with_options(
    list(vrtility.use_muparser = use_muparser),
    {
      hls_files <- fs::dir_ls(system.file("hls-data", package = "vrtility"))

      ex_collect <- vrt_collect(hls_files[1])

      ex_collect_mask_bit <- ex_collect |>
        vrt_set_maskfun(
          mask_band = "Fmask",
          mask_values = c(3, 2),
          build_mask_pixfun = build_bitmask(),
          drop_mask_band = FALSE
        )

      ex_nm <- sum(read_vals(ex_collect$vrt_src), na.rm = TRUE)
      ex_bm <- sum(read_vals(ex_collect_mask_bit$vrt_src), na.rm = TRUE)
      return(list(
        no_mask = ex_nm,
        mask_bit = ex_bm
      ))
    }
  )
}

intmask_test_vals <- function(use_muparser) {
  withr::with_options(
    list(vrtility.use_muparser = use_muparser),
    {
      s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

      ex_collect <- vrt_collect(s2files[3])

      ex_collect_mask_0buff <- ex_collect |>
        vrt_set_maskfun(
          mask_band = "SCL",
          mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
          build_mask_pixfun = build_intmask(),
          drop_mask_band = FALSE,
          buffer_size = 0
        )

      ex_collect_mask_10buff <- ex_collect |>
        vrt_set_maskfun(
          mask_band = "SCL",
          mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
          build_mask_pixfun = build_intmask(),
          drop_mask_band = FALSE,
          buffer_size = 3
        )

      # test that varying masks and buffers change the output
      ex_nm <- sum(read_vals(ex_collect$vrt_src), na.rm = TRUE)
      ex_0b <- sum(read_vals(ex_collect_mask_0buff$vrt_src), na.rm = TRUE)
      ex_10b <- sum(read_vals(ex_collect_mask_10buff$vrt_src), na.rm = TRUE)
      return(list(
        no_mask = ex_nm,
        mask_0buff = ex_0b,
        mask_10buff = ex_10b
      ))
    }
  )
}


test_that("vrt_set_maskfun works without muparser", {
  vals <- intmask_test_vals(use_muparser = FALSE)
  ex_nm <- vals$no_mask
  ex_0b <- vals$mask_0buff
  ex_10b <- vals$mask_10buff

  expect_gt(ex_nm, ex_0b)
  expect_gt(ex_0b, ex_10b)

  expect_error(set_mask(buffer_size = -1))
})

test_that("vrt_set_maskfun works with muparser", {
  skip_if(isFALSE(check_muparser()), "muparser not available")
  vals <- intmask_test_vals(use_muparser = TRUE)
  ex_nm <- vals$no_mask
  ex_0b <- vals$mask_0buff
  ex_10b <- vals$mask_10buff

  expect_gt(ex_nm, ex_0b)
  expect_gt(ex_0b, ex_10b)
})


test_that("muparser maskfuns are returning as expexted", {
  skip_if(isFALSE(check_muparser()), "muparser not available")
  bb_mp <- withr::with_options(
    list(vrtility.use_muparser = TRUE),
    build_bitmask()
  )
  expect_s3_class(bb_mp, "muparser_expression")

  expect_equal(
    as.character(bb_mp),
    "{paste0(\n  '(fmod(B1, ', 2^(mask_values + 1), ') >= ', 2^mask_values, ')',\n  collapse = ' || '\n)} ? 0 : 1"
  )

  bi_mp <- withr::with_options(
    list(vrtility.use_muparser = TRUE),
    build_intmask()
  )

  expect_s3_class(bi_mp, "muparser_expression")
  expect_equal(
    as.character(bi_mp),
    "({paste0('B1==', mask_values, collapse = ' || ')}) ? 0 : 1"
  )

  sm_mp <- withr::with_options(
    list(vrtility.use_muparser = TRUE),
    set_mask(buffer_size = 0) # this should actually return a python function.
  )
  expect_s3_class(sm_mp, "muparser_expression")
  expect_equal(
    as.character(sm_mp),
    "{bands[2]} != 0 ? {bands[1]} : NODATA"
  )

  sm_buff5 <- withr::with_options(
    list(vrtility.use_muparser = TRUE),
    set_mask(buffer_size = 5) # this should actually return a python function.
  )
  expect_s3_class(sm_buff5, "python_pixel_function")
  expect_equal(
    as.character(sm_buff5),
    "import numpy as np

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    buffer_size = 5

    if buffer_size > 0:
        from scipy import ndimage
        # Create mask (True = nodata)
        mask = in_ar[1] == 0

        # Buffer the mask
        structure = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
        buffered_mask = ndimage.binary_dilation(mask, structure=structure, iterations=buffer_size)

        # Apply buffered mask
        out_ar[:] = np.where(buffered_mask, no_data_val, in_ar[0])
    else:
        # no buffering
        out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)"
  )
})


test_that("python maskfuns are returning as expected", {
  bb_py <- withr::with_options(
    list(vrtility.use_muparser = FALSE),
    build_bitmask()
  )
  expect_s3_class(bb_py, "python_pixel_function")
  expect_equal(
    as.character(bb_py),
    "import numpy as np
def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    bit_positions = [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.zeros_like(in_ar[0], dtype=bool)
    for bit in bit_positions:
        mask |= np.bitwise_and(in_ar[0], np.left_shift(1, bit)) > 0
    out_ar[:] = np.where(mask, 0, 1)"
  )

  bi_py <- withr::with_options(
    list(vrtility.use_muparser = FALSE),
    build_intmask()
  )
  expect_s3_class(bi_py, "python_pixel_function")

  expect_equal(
    as.character(bi_py),
    "import numpy as np
def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    mask_vals =  [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.isin(in_ar[0], mask_vals)
    out_ar[:] = np.where(mask, 0, 1)"
  )

  sm_py <- withr::with_options(
    list(vrtility.use_muparser = FALSE),
    set_mask(buffer_size = 0)
  )
  expect_s3_class(sm_py, "python_pixel_function")
  expect_equal(
    as.character(sm_py),
    "import numpy as np

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    buffer_size = 0

    if buffer_size > 0:
        from scipy import ndimage
        # Create mask (True = nodata)
        mask = in_ar[1] == 0

        # Buffer the mask
        structure = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
        buffered_mask = ndimage.binary_dilation(mask, structure=structure, iterations=buffer_size)

        # Apply buffered mask
        out_ar[:] = np.where(buffered_mask, no_data_val, in_ar[0])
    else:
        # no buffering
        out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)"
  )

  sm_buff5 <- withr::with_options(
    list(vrtility.use_muparser = FALSE),
    set_mask(buffer_size = 5)
  )
  expect_s3_class(sm_buff5, "python_pixel_function")
  expect_equal(
    as.character(sm_buff5),
    "import numpy as np

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    buffer_size = 5

    if buffer_size > 0:
        from scipy import ndimage
        # Create mask (True = nodata)
        mask = in_ar[1] == 0

        # Buffer the mask
        structure = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
        buffered_mask = ndimage.binary_dilation(mask, structure=structure, iterations=buffer_size)

        # Apply buffered mask
        out_ar[:] = np.where(buffered_mask, no_data_val, in_ar[0])
    else:
        # no buffering
        out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)"
  )
})


test_that("bitmask results are the same with both implementations", {
  vals_mp <- bitmask_test_vals(use_muparser = TRUE)
  vals_py <- bitmask_test_vals(use_muparser = FALSE)

  expect_equal(vals_mp$no_mask, vals_py$no_mask)
  expect_equal(vals_mp$mask_bit, vals_py$mask_bit)
})
