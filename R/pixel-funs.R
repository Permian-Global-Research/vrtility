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
            # Convert zeros to nan
            values = np.where(values == 0, np.nan, values)
            result[y, x] = np.nanmedian(values)  # Much simpler!

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
