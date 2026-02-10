# Set the pixel function of a VRT stack object

Set the pixel function of a VRT stack object

## Usage

``` r
vrt_set_py_pixelfun(x, pixfun, band_idx)

# S3 method for class 'vrt_block'
vrt_set_py_pixelfun(x, pixfun = vrtility::median_numpy(), band_idx = NULL)

# S3 method for class 'vrt_collection'
vrt_set_py_pixelfun(x, pixfun = vrtility::median_numpy(), band_idx = NULL)

median_numpy()

mean_numpy()

geomean_numpy()

quantile_numpy(q, use_fastnanquantile = TRUE)

mean_db_numpy()
```

## Arguments

- x:

  A vrt_stack object

- pixfun:

  A function that returns the Python code for the pixel function

- band_idx:

  The indices of the bands to set the pixel function for. If NULL, the
  pixel function is set for all bands.

- q:

  Probability of the quantile to compute. Values must be between 0 and 1
  inclusive.

- use_fastnanquantile:

  Logical indicating whether to use the `fastnanquantile` library for
  calculating the quantile. This is generally faster than the standard
  numpy implementation, especially for large arrays. However, it
  requires the `fastnanquantile` library to be installed in the Python
  environment (This is handled automatically). Default is TRUE.

## Value

A modified object of the same class as `x` with the Python pixel
function set.

character of the python function

character of the python function

## Details

`median_numpy` is a pixel function that calculates the median of the
input arrays, it is injected into the VRT file as a Python function.
`mean_numpy` works in the same way but calculates the mean.\`

`geomean_numpy` is a pixel function that calculates the geometric mean
of the input arrays. Use cases of this are at present unclear to me. If
you have thoughts or references please let me know.

`quantile_numpy` is a pixel function that calculates the quantile of the
input arrays for a given probability. This could be useful where the
median fails to filter cloudy pixels effectively. The defauly numpy
nanquantile function is very slow and does not support masked arrays.
Therefore, it is typically much faster to use the fastnanquantile
library which uses numba to increase performance. However, this library
has some limitations around multi-threading in certain contexts.
Therefore, when using this pixel function with
[`vrt_compute()`](https://permian-global-research.github.io/vrtility/reference/vrt_compute.md),
it is strongly advised to use the `warp` engine and to avoid using mirai
daemons (i.e. use sequential processing).

`mean_db_numpy` is a pixel function that calculates the mean of the
input arrays and then converts to decibels. This is useful for
calculating the mean of radar raw/linear backscatter values, for
example.
