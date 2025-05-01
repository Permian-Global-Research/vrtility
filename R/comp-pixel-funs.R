#' @return character of the python function
#' @rdname vrt_set_pixelfun
#' @details `median_numpy` is a pixel function that calculates the median of
#' the input arrays, it is injected into the VRT file as a Python function.
#' `mean_numpy` works in the same way but calculates the mean.`
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
    out_ar[:] = np.ma.median(masked_data, axis=0).filled(fill_value=no_data_val)
"
  )
}

#' python pixel function for the mean using numpy
#' @return character of the python function
#' @rdname vrt_set_pixelfun
#' @export
mean_numpy <- function() {
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
    
    # Calculate mean on masked array directly
    out_ar[:] = np.ma.mean(masked_data, axis=0).filled(fill_value=no_data_val)
}
"
  )
}
