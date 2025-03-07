#' Masking functions for pixel-based processing
#' @export
#' @details This function is a simple bit mask function that can be used to
#' mask out pixels based on a bit mask. There is no bitwise transformation
#' applied to the provided valid_bits.
#' @rdname vrt_set_maskfun
bitmask_numpy <- function() {
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

#' Masking functions for pixel-based processing
#' @export
#' @details This function is a simple bit mask function that can be used to
#' mask out pixels based on a bit mask. There is no bitwise transformation
#' applied to the provided valid_bits.
#' @rdname vrt_set_maskfun
bitmask_numba <- function() {
  glue::glue(
    "
import numpy as np
import numba
from numba import prange

@numba.jit(nopython=True, parallel=True)
def apply_mask(data, mask_data, no_data_val):
    out = np.full_like(data, no_data_val)  # Initialize with no_data_val
    rows, cols = data.shape
    # Use prange for outer loop
    for i in prange(rows):  
        for j in range(cols):
            # Simple mask: if mask_data > 0, keep the original value
            if mask_data[i,j] > 0:
                out[i,j] = data[i,j]
    return out

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    no_data_val = int(kwargs['no_data_value'])
    out_ar[:] = apply_mask(in_ar[0], in_ar[1], no_data_val)
"
  )
}


#' Masking functions for pixel-based processing
#' @details This function is a simple bit mask function that can be used to
#' mask out pixels based on a bit mask. There is no bitwise transformation
#' applied to the provided valid_bits.
#' @keywords internal
#' @noRd
build_bitmask_numpy <- function() {
  glue::glue(
    "
import numpy as np
def build_bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]

    mask = np.isin(in_ar[0], valid_vals)
    # breakpoint()
    out_ar[:] = np.where(mask, 255, 0)  # Set invalid pixels to 0
"
  )
}
