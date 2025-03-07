#' python pixel function for the median using numba
#' @return character of the python function
#' @export
median_numba <- function() {
  glue::glue(
    "\n
import numpy as np
from numba import jit, prange

@jit(nopython=True, parallel=True)
def median_jit(in_ar, no_data_val):
    result = np.empty(in_ar.shape[1:], dtype=np.float32)

    for y in prange(in_ar.shape[1]):
        for x in range(in_ar.shape[2]):
            values = in_ar[:, y, x]  # Get all values for this pixel
            # Convert zeros to nan
            values = np.where(values == no_data_val, np.nan, values)
            result_with_nan = np.nanmedian(values)
            
            result[y, x] = np.where(
              np.isnan(result_with_nan), 
              no_data_val, 
              result_with_nan
            )

    return result

def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
          raster_ysize, buf_radius, gt, **kwargs):
    
    no_data_val = int(kwargs['no_data_value'])
    stacked = np.stack(in_ar)
    out_ar[:] = median_jit(stacked, no_data_val)
"
  )
}

#' python pixel function for the median using numpy
#' @return character of the python function
#' @export
median_numpy <- function() {
  glue::glue(
    "
import numpy as np
def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
    no_data_val = int(kwargs['no_data_value'])
    # Stack the input arrays first
    stacked = np.stack(in_ar)
    
    # Create a mask of no_data values
    mask = (stacked == no_data_val)
    
    # Create a masked array - more efficient than np.where for this operation
    masked_data = np.ma.array(stacked, mask=mask, shrink=False)
    
    # Calculate median on masked array directly
    out_ar[:] = np.ma.median(masked_data, axis=0)
"
  )
}
