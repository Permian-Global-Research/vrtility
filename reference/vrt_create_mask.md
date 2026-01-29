# Create a new mask for a vrt object This function allows you to create a mask for a VRT object based on specified input bands and a mask function.

This function constructs a cloud mask using the OmniCloudMask python
library. It is designed to be used with the
[`vrt_set_maskfun()`](https://permian-global-research.github.io/vrtility/reference/vrt_set_maskfun.md)
function.

## Usage

``` r
vrt_create_mask(x, inbands, maskfun, nodata_value = 0)

create_omnicloudmask(
  patch_size = 1000,
  patch_overlap = 300,
  batch_size = 1,
  inference_dtype = c("bfloat16", "float32"),
  inference_device = NULL,
  nodata_value = 0,
  model_version = NULL
)
```

## Arguments

- x:

  A VRT_x object.

- inbands:

  A named numeric vector of input band indices to be used for the mask.

- maskfun:

  A character containing a python pixel function to be applied to the
  input bands to create the mask. Currently the only provided function
  is `create_omnicloudmask`. This string must also have three key
  attributes: `mask_name`, `mask_description`, and `required_bands`.

- nodata_value:

  The nodata value to use in the output mask (numeric, default: 0).

- patch_size:

  The size/dimension of the patches to use for prediction (numeric
  default: 600).

- patch_overlap:

  The overlap between patches (numeric default: 300).

- batch_size:

  The batch size to use for prediction (numeric default: 1).

- inference_dtype:

  The data type to use for inference. Options include "bfloat16" and
  "float32" (character, default: "bfloat16"). using "bfloat16" should be
  faster if supported by the hardware.

- inference_device:

  The device to use for inference. If `NULL`, the function will
  automatically select the best available device (character, default:
  `NULL`). Options include "cpu", "cuda", "mps", etc. The order of
  selection is based on availability: "cuda" \> "mps" \> "cpu".

- model_version:

  The version of the OmniCloudMask model to use; options include "4.0",
  "3.0", "2.0", and "1.0". If `NULL`, the latest version will be used
  (character, default: `NULL`).

## Value

A VRT_x object with the the new mask band added.

A Python function that can be used as a pixel function in a VRT raster.
The function will apply the OmniCloudMask model to the specified bands
and create a cloud mask.

## References

OmniCloudMask GitHub repository:
<https://github.com/DPIRD-DMA/OmniCloudMask>

## Examples

``` r
if (FALSE) { # interactive()
 s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
 ex_collect <- vrt_collect(s2files[4:5])

 ex_mask <- vrt_create_mask(
   ex_collect,
   c(red = 3, green = 2, nir = 4),
   maskfun = create_omnicloudmask()
 )

 ex_mask_compute <- vrt_compute(
   ex_mask,
   fs::file_temp(ext = ".tif"),
   recollect = TRUE
 )
 plot(ex_mask_compute, item = 2, bands =   6)
}
```
