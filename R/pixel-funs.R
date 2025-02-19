#' python pixel function for the median
#' @return character
#' @keywords internal
#' @noRd
numba_median <- function() {
  glue::glue("\n
import numpy as np
from numba import jit, prange

@jit(nopython=True, parallel=True)
def median_jit(in_ar):
    result = np.empty(in_ar.shape[1:], dtype=np.float32)

    for y in prange(in_ar.shape[1]):
        for x in prange(in_ar.shape[2]):
            values = in_ar[:, y, x]  # Get all values for this pixel
            valid_vals = values[~np.isnan(values)]  # Filter out NaNs

            if len(valid_vals) == 0:
                result[y, x] = np.nan
            else:
                valid_vals.sort()
                mid = len(valid_vals) // 2
                if len(valid_vals) % 2 == 0:
                    result[y, x] = (valid_vals[mid-1] + valid_vals[mid]) / 2
                else:
                    result[y, x] = valid_vals[mid]

    return result

def median(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
          raster_ysize, buf_radius, gt, **kwargs):
    try:
        # Stack arrays into 3D array (sources, y, x)
        stacked = np.stack([arr for arr in in_ar])
        out_ar[:] = median_jit(stacked)
    except Exception as e:
        print(f'Error in median function: {{str(e)}}')
        raise
")
}
