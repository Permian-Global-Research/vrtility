# Image processing along a time series.

`singleband_m2m` can be used to filter raster time series' or apply
smoothing operations. These functions work on a single band at a time.
composite of a warped VRT collection.

`hampel_filter` is used tocreate a function to filter band-level
outliers in time-series using the Hampel filter. to be provided to
`singleband_m2m()`.

## Usage

``` r
singleband_m2m(
  x,
  m2m_fun,
  outfile,
  config_options,
  creation_options,
  quiet,
  nsplits,
  recollect
)

hampel_filter(k = 1L, t0 = 3, impute_na = FALSE)
```

## Arguments

- x:

  A vrt_collection_warped object.

- m2m_fun:

  A function to apply to the data. This function should take a matrix as
  input and return a matrix as output. The function should be vectorized
  over the rows of the matrix, where each row represents a time and
  columns represent pixels. calcualtions are done at the band level.

- outfile:

  The output file path.

- config_options:

  A named character vector of GDAL configuration options.

- creation_options:

  A named character vector of GDAL creation options.

- quiet:

  Logical indicating whether to suppress the progress bar.

- nsplits:

  The number of splits to use for the tiling. If NULL, the function will
  automatically determine the number of splits based on the dimensions
  of the input data, available memory and the number of active mirai
  daemons. see details

- recollect:

  A logical indicating whether to return the output as a vrt_block or
  vrt_collection object. default is FALSE and the output is a character
  string of the output file path.

- k:

  The number of neighboring points to consider on each side of the
  current point. window length 2\*k+1 in indices.

- t0:

  threshold, default is 3 (Pearson's rule), see below.

- impute_na:

  Logical indicating whether to impute NA values. If `TRUE`, the
  function will impute NA values using the nearest prior non-NA value.
  If `FALSE`, NA values will be returned in their original positions.

## Value

A character vector of the output file path.

A function to be used with `singleband_m2m()` to remove outliers from a
raster time series.

## Details

We have a lot TODO: here...

(details from the `pracma::hampel`) The ‘median absolute deviation’
computation is done in the (-k...k) vicinity of each point at least k
steps away from the end points of the interval. At the lower and upper
end the time series values are preserved.

A high threshold makes the filter more forgiving, a low one will declare
more points to be outliers. t0\<-3 (the default) corresponds to Ron
Pearson's 3 sigma edit rule, t0\<-0 to John Tukey's median filter.

The implementation of the hampel filter is based on the pracma package
but implemented in C++. It also handles NA values differently, First, NA
values are removed from the data before applying the filter. If
`impute_na` is `TRUE`, the function will impute NA values using the
nearest prior non-NA value. If `impute_na` is `FALSE`, NA values will be
returned in their original positions.
