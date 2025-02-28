#' python pixel function for the median using numba
#' @return character of the python function
#' @export
numba_median <- function() {
  glue::glue(
    "\n
import numpy as np
from numba import jit, prange

@jit(nopython=True, parallel=True)
def median_jit(in_ar, no_data_val):
    result = np.empty(in_ar.shape[1:], dtype=np.float32)

    for y in prange(in_ar.shape[1]):
        for x in prange(in_ar.shape[2]):
            values = in_ar[:, y, x]  # Get all values for this pixel
            # Convert zeros to nan
            values = np.where(values == no_data_val, np.nan, values)
            result_with_nan = np.nanmedian(values)
            
            result[y, x] = np.where(np.isnan(result_with_nan), no_data_val, result_with_nan)

    return result

def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
          raster_ysize, buf_radius, gt, **kwargs):
    
    no_data_val = int(kwargs['no_data_value'])
    try:
        stacked = np.stack(in_ar)
        out_ar[:] = median_jit(stacked, no_data_val)
    except Exception as e:
        print('Error in median function: %s' % str(e))
        raise
"
  )
}

#' python pixel function for the median using numpy
#' @return character of the python function
#' @export
numpy_median <- function() {
  glue::glue(
    "
import numpy as np
def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
    no_data_val = int(kwargs['no_data_value'])
    # Stack the input arrays first
    stacked = np.stack(in_ar)
    # Now use the stacked array's dtype
    no_data = np.array(no_data_val, dtype=stacked.dtype)
    in_ar_nan = np.where(stacked == no_data, np.nan, stacked)
    out_ar[:] = np.nanmedian(in_ar_nan, axis=0)
"
  )
}
