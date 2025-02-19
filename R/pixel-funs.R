#' python pixel function for the median
#' @return character
#' @keywords internal
#' @noRd
numba_median <- function() {
  glue::glue("\n
import numpy as np
from numba import jit, prange

@jit(nopython=True, parallel=True, fastmath=True)
def median_jit(in_ar):
    result = np.empty(in_ar.shape[1:], dtype=np.float32)
    rows, cols = in_ar.shape[1:]

    # Process by rows for better cache utilization
    for y in prange(rows):
        for x in prange(cols):  # No prange here - avoid thread overhead
            values = in_ar[:, y, x]
            result[y, x] = np.nanmedian(values)

    return result

def median(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
          raster_ysize, buf_radius, gt, **kwargs):
    try:
        stacked = np.stack(in_ar)
        out_ar[:] = median_jit(stacked)
    except Exception as e:
        print('Error in median function: %s' % str(e))
        raise
")
}


numpy_median <- function() {
  glue::glue("
import numpy as np
def median(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
    out_ar[:] = np.nanmedian(in_ar, axis=0)
")
}


sentinel_mask <- function() {
  glue::glue("
import numpy as np

def mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
    # Valid SCL values:
    # 4 = Vegetation
    # 5 = Not Vegetated
    # 6 = Water
    # 7 = Unclassified
    # 11 = Snow
    valid_values = np.array([4, 5, 6, 7, 11], dtype=np.uint8)
    out_ar[:] = np.isin(in_ar[0], valid_values).astype(np.float32)
")
}
