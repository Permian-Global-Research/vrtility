#' Masking functions VRT pixel functions.
#' @param buffer_size A buffer size to apply to the mask (numeric, default: 0).
#' A buffer
#' size > 0 will dilate the mask by the specified number of pixels.
#' This can be useful to remove edge effects around clouds.
#' If a buffer size > 0 is specified, the `scipy` python library will
#' automatically be installed.
#' @param use_muparser Logical. If `TRUE` and GDAL is built with muparser
#' support (GDAL >= 3.12.0), uses muparser expression instead of Python.
#' @noRd
#' @keywords internal
#' @details `set_mask` simply applies a given mask where values of 0 are
#' assumed to have nodata and values > 0  contain valid data.
#' @rdname vrt_set_maskfun
set_mask <- function(
  buffer_size = 0,
  use_muparser = getOption("vrtility.use_muparser", FALSE)
) {
  v_assert_type(
    buffer_size,
    "buffer_size",
    c("numeric", "integer"),
    multiple = TRUE
  )

  v_assert_within_range(
    buffer_size,
    "buffer_size",
    0,
    Inf
  )

  if (buffer_size == 0 && check_muparser("3.12.0") && use_muparser) {
    return(set_mask_muparser())
  }

  if (buffer_size > 0) {
    # assert omicloudmask is installed
    vrtility_py_require("scipy")
  }

  return(set_mask_python(buffer_size))
}

#' @details `set_mask_muparser` provides a muparser expression for simple masking
#' where mask values of 0 indicate nodata and values > 0 indicate valid data.
#' This is faster than the Python version when buffering is not needed, and
#' doesn't require Python dependencies.
#' @noRd
#' @keywords internal
set_mask_muparser <- function() {
  e <- "{bands[2]} != 0 ? {bands[1]} : NODATA"
  class(e) <- c("muparser_expression", class(e))
  return(e)
}

#' @param buffer_size A buffer size to apply to the mask (numeric, default: 0). A buffer
#' size > 0 will dilate the mask by the specified number of pixels.
#' This can be useful to remove edge effects around clouds.
#' If a buffer size > 0 is specified, the `scipy` python library will
#' automatically be installed.
#' @details `set_mask_python` provides a Python pixel function for masking
#' where mask values of 0 indicate nodata and values > 0 indicate valid data.
#' This function also supports buffering of the mask using a specified buffer
#' size.
#' @noRd
#' @keywords internal
set_mask_python <- function(buffer_size) {
  pf <- glue::glue(
    "
import numpy as np

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    buffer_size = {buffer_size}

    if buffer_size > 0:
        from scipy import ndimage
        # Create mask (True = nodata)
        mask = in_ar[1] == 0

        # Buffer the mask
        structure = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
        buffered_mask = ndimage.binary_dilation(mask, structure=structure, iterations=buffer_size)

        # Apply buffered mask
        out_ar[:] = np.where(buffered_mask, no_data_val, in_ar[0])
    else:
        # no buffering
        out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)
"
  )
  class(pf) <- c("python_pixel_function", class(pf))
  return(pf)
}


#' @details `build_intmask` provides an integer mask function that can be used
#' to mask out pixels based on a band containing true integer/numeric values.
#' This would be appropriate for the Sentinel 2A SCL band, for example.
#' @param use_muparser Logical. If `TRUE` and GDAL >= 3.12, uses muparser
#' expression instead of Python. Default is to auto-detect based on GDAL version.
#' @export
#' @rdname vrt_set_maskfun
build_intmask <- function(
  use_muparser = getOption("vrtility.use_muparser", FALSE)
) {
  v_assert_type(
    use_muparser,
    "use_muparser",
    "logical",
    nullok = FALSE
  )

  if (use_muparser) {
    if (!check_muparser("3.11.4")) {
      muparser_mask_warn("build_intmask")
      return(build_intmask_python())
    }
    return(build_intmask_muparser())
  }

  return(build_intmask_python())
}

build_intmask_python <- function() {
  pf <- glue::glue(
    "
import numpy as np
def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    mask_vals =  [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.isin(in_ar[0], mask_vals)
    out_ar[:] = np.where(mask, 0, 1)
"
  )
  class(pf) <- c("python_pixel_function", class(pf))
  return(pf)
}


#' @details `build_intmask_muparser` provides a muparser expression template for
#' integer masking where specified values indicate invalid data (set to 0 in mask).
#' Returns a glue template that will be filled with mask_values later.
#' @noRd
#' @keywords internal
build_intmask_muparser <- function() {
  # Return a glue template that will be filled with mask_values
  # The expression checks if B1 equals any mask value
  expr <- "({paste0('B1==', mask_values, collapse = ' || ')}) ? 0 : 1"

  class(expr) <- c("muparser_expression", class(expr))
  return(expr)
}


#' @details `build_bitmask` provides is a simple bit-wise mask function that can
#' be used to mask out pixels based on a true bit mask. This function should be
#' used where bitwise operations are required. e.g. for HLS data, the "Fmask"
#' band requires bitwise operations to extract the mask values.
#' @param use_muparser Logical. If `TRUE` and GDAL >= 3.12, uses muparser
#' @export
#' @rdname vrt_set_maskfun
build_bitmask <- function(
  use_muparser = getOption("vrtility.use_muparser", FALSE)
) {
  v_assert_type(
    use_muparser,
    "use_muparser",
    "logical",
    nullok = FALSE
  )

  if (!use_muparser) {
    return(build_bitmask_python())
  }

  if (!check_muparser()) {
    muparser_mask_warn("build_bitmask")
    return(build_bitmask_python())
  }

  return(build_bitmask_muparser())
}

#' @details `build_bitmask_python` provides a Python pixel function for bitwise
#' masking where specified bit positions indicate invalid data.
#' @noRd
#' @keywords internal
build_bitmask_python <- function() {
  pf <- glue::glue(
    glue::glue(
      "
import numpy as np
def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    bit_positions = [int(x) for x in kwargs['mask_values'].decode().split(',')]
    mask = np.zeros_like(in_ar[0], dtype=bool)
    for bit in bit_positions:
        mask |= np.bitwise_and(in_ar[0], np.left_shift(1, bit)) > 0
    out_ar[:] = np.where(mask, 0, 1)
"
    )
  )
  class(pf) <- c("python_pixel_function", class(pf))
  return(pf)
}

#' @details `build_bitmask_muparser` provides a muparser expression template for
#' bitwise masking where specified bit positions indicate invalid data.
#' Returns a glue template that will be filled with mask_values later.
#' @noRd
#' @keywords internal
build_bitmask_muparser <- function() {
  # Return a glue template that will be filled with mask_values
  # The expression checks if any specified bits are set
  # For each bit: (B1 & (1 << bit)) > 0
  # Need to wrap paste0 call properly and ensure parentheses are correct
  expr <- "{paste0(
  '(fmod(B1, ', 2^(mask_values + 1), ') >= ', 2^mask_values, ')',
  collapse = ' || '
)} ? 0 : 1"
  class(expr) <- c("muparser_expression", class(expr))
  return(expr)
}

# NOTE: we use 1 not 255 as a valid data value in the mask because HLS data uses
# 255 as no data, which results in masking the entire raster. This is becuase
# we apply the masks in an unconventional way using pixel functions and not
# using the RFC 15 approach. I've tried various approaches to get this to work
# but it seems that the RFC 15 approach is not compatible with a mask containing
# pixel functions. If anyone has a solution to this please let me know.

# nolint

#' @title Construct a cloud mask using the OmniCloudMask python library.
#' @description This function constructs a cloud mask using the OmniCloudMask
#' python library. It is designed to be used with the `vrt_set_maskfun()`
#' function.
#' @param patch_size The size/dimension of the patches to use for prediction
#' (numeric default: 600).
#' @param patch_overlap The overlap between patches (numeric default: 300).
#' @param batch_size The batch size to use for prediction (numeric default: 1).
#' @param inference_dtype The data type to use for inference. Options include
#' "bfloat16" and "float32" (character, default: "bfloat16"). using "bfloat16"
#' should be faster if supported by the hardware.
#' @param inference_device The device to use for inference. If `NULL`, the
#' function will automatically select the best available device (character,
#' default: `NULL`). Options include "cpu", "cuda", "mps", etc. The order of
#' selection is based on availability: "cuda" > "mps" > "cpu".
#' @param nodata_value The nodata value to use in the output mask (numeric,
#' default: 0).
#' @param model_version The version of the OmniCloudMask model to use; options
#' include "4.0", "3.0", "2.0", and "1.0". If `NULL`, the latest version will be
#' used (character, default: `NULL`).
#' @return A Python function that can be used as a pixel function in a VRT
#' raster. The function will apply the OmniCloudMask model to the specified
#' bands and create a cloud mask.
#' @rdname vrt_create_mask
#' @references
#' OmniCloudMask GitHub repository: \url{https://github.com/DPIRD-DMA/OmniCloudMask}
#' @export
create_omnicloudmask <- function(
  patch_size = 1000,
  patch_overlap = 300,
  batch_size = 1,
  inference_dtype = c("bfloat16", "float32"),
  inference_device = NULL,
  nodata_value = 0,
  model_version = NULL
) {
  # assert omicloudmask is installed
  if (!reticulate::py_module_available("omnicloudmask")) {
    vrtility_py_require("omnicloudmask")
  }

  if (!reticulate::py_module_available("fastai")) {
    vrtility_py_require("fastai")
  }

  # assert args
  v_assert_type(
    patch_size,
    "patch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    patch_overlap,
    "patch_overlap",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    batch_size,
    "batch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )

  if (!is.null(inference_device)) {
    inference_device <- rlang::arg_match(
      inference_device,
      c(
        "cpu",
        "cuda",
        "ipu",
        "xpu",
        "mkldnn",
        "opengl",
        "opencl",
        "ideep",
        "hip",
        "ve",
        "fpga",
        "maia",
        "xla",
        "lazy",
        "vulkan",
        "mps",
        "meta",
        "hpu",
        "mtia",
        "privateuseone"
      )
    )
  } else {
    torch <- reticulate::import("torch")
    if (torch$cuda$is_available()) {
      inference_device <- "cuda"
    } else if (torch$backends$mps$is_available()) {
      inference_device <- "mps"
    } else {
      inference_device <- "cpu"
    }
  }

  inference_dtype <- rlang::arg_match(inference_dtype)

  if (!is.null(model_version)) {
    model_version <- rlang::arg_match(
      model_version,
      c("4.0", "3.0", "2.0", "1.0")
    )
  } else {
    model_version <- "None"
  }

  pyfun <- glue::glue(
    "
import numpy as np
import omnicloudmask as omc
import torch

def create_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    try:
      np_rgn = np.stack([in_ar[0], in_ar[1], in_ar[2]], axis=0)
      
      pred_mask = omc.predict_from_array(
        np_rgn,
        patch_size = {patch_size},
        patch_overlap = {patch_overlap},
        batch_size = {batch_size},
        inference_device = '{inference_device}',
        inference_dtype = torch.{inference_dtype},
        no_data_value = {nodata_value},
        model_version = {model_version}
      ) 

      out_ar[:] = pred_mask[0]

    finally:
      if '{inference_device}' == 'cuda':
        torch.cuda.empty_cache()
      elif '{inference_device}' == 'mps':
        torch.backends.mps.empty_cache()
"
  )

  # TODO: maybe this should just be a class?
  attr(pyfun, "mask_name") <- "create_omnicloudmask"
  attr(pyfun, "mask_description") <- "omnicloudmask"
  attr(pyfun, "required_bands") <- c("red", "green", "nir")
  return(pyfun)
}
