#' Masking functions VRT pixel functions.
#' @export
#' @details `set_mask_numpy` simply applies a given mask where values of 0 are
#' assumed to have nodata and values > 0 (typically 255) contain valid data.
#' It is the only provided function for the `set_mask_pixfun` argument in
#' `vrt_set_maskfun()`. Alternatively a custom function could be provided if,
#' for example a user wishes to buffer the mask.
#' @rdname vrt_set_maskfun
set_mask_numpy <- function() {
  glue::glue(
    "
import numpy as np
def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)
    
"
  )
}


#' @details `build_intmask` provides an integer mask function that can be used
#' to mask out pixels based on a band containing true integer/numeric values.
#' This would be appropriate for the Sentinel 2A SCL band, for example.
#' @export
#' @rdname vrt_set_maskfun
build_intmask <- function() {
  glue::glue(
    "
import numpy as np
def build_bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    mask_vals =  [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.isin(in_ar[0], mask_vals)
    out_ar[:] = np.where(mask, 0, 1)  # Set invalid pixels to 0
"
  )
}


#' @details `build_bitmask` provides is a simple bit-wise mask function that can
#' be used to mask out pixels based on a true bit mask. This function should be
#' used where bitwise operations are required. e.g. for HLS data, the "Fmask"
#' band requires bitwise operations to extract the mask values.
#' @export
#' @rdname vrt_set_maskfun
build_bitmask <- function() {
  glue::glue(
    "
import numpy as np
def build_bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    bit_positions = [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.zeros_like(in_ar[0], dtype=bool)
    for bit in bit_positions:
        mask |= np.bitwise_and(in_ar[0], np.left_shift(1, bit)) > 0
    out_ar[:] = np.where(mask, 0, 1)
"
  )
}

# NOTE: we use 1 not 255 as a valid data value in the mask because HLS data uses
# 255 as no data, which results in masking the entire raster. This is becuase
# we apply the masks in an unconventional way using pixel functions and not
# using the RFC 15 approach. I've tried various approaches to get this to work
# but it seems that the RFC 15 approach is not compatible with a mask containing
# pixel functions. If anyone has a solution to this please let me know.

# nolint

#
build_omnicloudmask <- function(
  r,
  g,
  nir,
  patch_size = 1000,
  patch_overlap = 300,
  batch_size = 1,
  no_data_value = 0,
  inference_device = NULL
) {
  # assert omicloudmask is installed
  omc <- try(
    reticulate::import("omnicloudmask", delay_load = TRUE),
    silent = TRUE
  )

  if (inherits(omc, "try-error")) {
    vrtility_py_require("omnicloudmask")
  }

  # assert args
  v_assert_type(r, "r", c("numeric", "integer"), multiple = TRUE)
  v_assert_type(g, "g", c("numeric", "integer"), multiple = TRUE)
  v_assert_type(nir, "nir", c("numeric", "integer"), multiple = TRUE)
  v_assert_type(
    patch_size,
    "patch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    patch_overlap,
    "patch_overlap",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    batch_size,
    "batch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(no_data_value, "no_data_value", c("numeric"))

  if (!is.null(inference_device)) {
    inference_device <- rlang::arg_match(
      inference_device,
      c(
        "cpu",
        "cuda",
        "ipu",
        "xpu",
        "mkldnn",
        "opengl",
        "opencl",
        "ideep",
        "hip",
        "ve",
        "fpga",
        "maia",
        "xla",
        "lazy",
        "vulkan",
        "mps",
        "meta",
        "hpu",
        "mtia",
        "privateuseone"
      )
    )
  } else {
    torch <- reticulate::import("torch")
    if (torch$cuda$is_available()) {
      inference_device <- "cuda"
    } else if (torch$backends$mps$is_available()) {
      inference_device <- "mps"
    } else {
      inference_device <- "cpu"
    }
  }

  pyfun <- glue::glue(
    "
import numpy as np
import omnicloudmask as omc



def build_bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    # breakpoint()
    mask_vals = [int(x) for x in kwargs['mask_values'].decode().split(',')]

    
    np_rgn = np.stack([in_ar[0], in_ar[1], in_ar[2]], axis=0)

    
    pred_mask = omc.predict_from_array(
      np_rgn,
      patch_size = {patch_size},
      patch_overlap = {patch_overlap},
      batch_size = {batch_size},
      inference_device = '{inference_device}',
      no_data_value = {no_data_value}
    ) 

    # mask = np.isin(pred_mask[0], mask_vals)
    out_ar[:] = pred_mask[0] # np.where(mask, 0, 1)  # Set invalid pixels to 0
    


"
  )

  attr(pyfun, "mask_type") <- "omnicloudmask"
  attr(pyfun, "required_bands") <- c(r, g, nir)
  return(pyfun)
}
