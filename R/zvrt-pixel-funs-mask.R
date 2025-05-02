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
