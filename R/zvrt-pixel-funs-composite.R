#' @return character of the python function
#' @rdname vrt_set_py_pixelfun
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

#' @return character of the python function
#' @rdname vrt_set_py_pixelfun
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
"
  )
}

#' @details `geomean_numpy` is a pixel function that calculates the geometric
#' mean of the input arrays. Use cases of this are at present unclear to me.
#' If you have thoughts or references please let me know.
#' @rdname vrt_set_py_pixelfun
#' @export
geomean_numpy <- function() {
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
    out_ar[:] = np.ma.exp(np.ma.mean(np.ma.log(masked_data), axis=0)).filled(fill_value=no_data_val)
"
  )
}

#' @param q Probability of the quantile to compute. Values must be between 0 and 1
#' inclusive.
#' @details `quantile_numpy` is a pixel function that calculates the quantile
#' of the input arrays for a given probability. This could be useful where
#' the median fails to filter cloudy or shadowy images effectively.
#' @rdname vrt_set_py_pixelfun
#' @export
quantile_numpy <- function(q) {
  v_assert_type(q, "q", "numeric", nullok = FALSE)
  v_assert_within_range(q, "q", 0, 1)

  glue::glue(
    "
import numpy as np
import warnings
def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
    no_data_val = int(kwargs['no_data_value'])
    # Stack the input arrays first
    stacked = np.stack(in_ar)

    # Set nodata values to np.nan
    stacked = np.where(stacked == no_data_val, np.nan, stacked)
    
    with warnings.catch_warnings():
      warnings.simplefilter('ignore', category=RuntimeWarning)
      out_ar[:] = np.nanquantile(stacked, {q}, axis=0)
      out_ar[np.isnan(out_ar)] = no_data_val
"
  )
}

#' @details `mean_db_numpy` is a pixel function that calculates the mean of
#' the input arrays and then converts to decibels. This is useful for
#' calculating the mean of radar raw/linear backscatter values, for example.
#' @rdname vrt_set_py_pixelfun
#' @export
mean_db_numpy <- function() {
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
    mean_vals = np.ma.mean(masked_data, axis = 0)
    db_vals = 10 * np.ma.log10(mean_vals)
    out_ar[:] = db_vals.filled(fill_value=no_data_val)
"
  )
}
