#' Masking functions for pixel-based processing
#' @export
#' @details This function is a simple bit mask function that can be used to
#' mask out pixels based on a bit mask. There is no bitwise transformation
#' applied to the provided valid_bits.
#' @rdname vrt_set_mask
bitmask_numpy <- function() {
  glue::glue(
    "
import numpy as np
def sentinel2_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    # Get data and mask arrays
    data = in_ar[0]
    mask_data = in_ar[1]
    
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]

    # Create mask and apply to data
    mask = np.isin(mask_data, valid_vals)
    out_ar[:] = np.where(mask, data, 0)  # Set invalid pixels to 0
"
  )
}

#' Masking functions for pixel-based processing
#' @export
#' @details This function is a simple bit mask function that can be used to
#' mask out pixels based on a bit mask. There is no bitwise transformation
#' applied to the provided valid_bits.
#' @rdname vrt_set_mask
bitmask_numba <- function() {
  glue::glue(
    "
import numpy as np
import numba
from numba import prange  # Add prange import

@numba.jit(nopython=True, parallel=True)
def apply_mask(data, mask_data, valid_vals):
    out = np.zeros_like(data)
    rows, cols = data.shape
    # Use prange for outer loop
    for i in prange(rows):  
        for j in range(cols):
            mask_val = mask_data[i,j]
            # Manual check for valid values
            for val in valid_vals:
                if mask_val == val:
                    out[i,j] = data[i,j]
                    break
    return out

def sentinel2_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    data = in_ar[0]
    mask_data = in_ar[1]
    valid_vals = np.array([int(x) for x in kwargs['valid_values'].decode().split(',')], 
                         dtype=np.int32)
    out_ar[:] = apply_mask(data, mask_data, valid_vals)
"
  )
}
