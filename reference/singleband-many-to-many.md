# Temporal filtering and smoothing for raster time series

Apply temporal operations to each pixel's time series in a warped VRT
collection. This function processes each band independently, applying a
user-defined function across the temporal dimension (e.g., filtering
outliers, smoothing, gap-filling).

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

  A `vrt_collection_warped` object. All images must be aligned to the
  same grid (use
  [`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
  first).

- m2m_fun:

  A function that operates on time series data. The function receives a
  matrix where rows represent time steps and columns represent pixels,
  and must return a matrix of the same dimensions. See `hampel_filter`
  for an example. Calculations are performed independently for each
  band.

- outfile:

  Output file path(s). Can be a single path (used as template) or a
  vector of paths (one per time step). Defaults to temporary files.

- config_options:

  A named character vector of GDAL configuration options. See
  [`gdal_config_opts()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md).

- creation_options:

  A named character vector of GDAL creation options for output files.
  See
  [`gdal_creation_options()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md).

- quiet:

  Logical. If `FALSE`, displays a progress bar during processing.

- nsplits:

  Integer specifying the number of spatial tiles to process. If `NULL`
  (default), automatically determines optimal tiling based on input
  dimensions, available memory, and number of parallel workers.

- recollect:

  Logical. If `TRUE`, returns a `vrt_collection` object containing the
  processed images. If `FALSE` (default), returns a character vector of
  output file paths.

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

If `recollect = FALSE`, a character vector of output file paths (one per
time step). If `recollect = TRUE`, a `vrt_collection` object.

A function to be used with `singleband_m2m()` to remove outliers from a
raster time series.

## Details

### Processing Model

`singleband_m2m()` materializes the virtual raster collection by:

1.  **Spatial tiling**: Divides the spatial extent into tiles for
    efficient memory usage

2.  **Band-wise processing**: For each band and tile, extracts the
    complete time series (all images)

3.  **Temporal operations**: Applies `m2m_fun` to the time series matrix
    (rows = time, cols = pixels)

4.  **Output writing**: Writes filtered results to disk as GeoTIFF files

### Common Use Cases

- **Outlier removal**: Remove cloud contamination or sensor artifacts
  using `hampel_filter()`

- **Temporal smoothing**: Apply moving average or median filters

- **Gap filling**: Interpolate missing values in time series

- **Temporal compositing**: Create median/mean composites while
  preserving all time steps

### Performance Considerations

- Processing is parallelized if `mirai` daemons are active (see
  `daemons`[mirai::mirai](https://mirai.r-lib.org/reference/mirai.html)).

- Automatic tiling balances memory usage against I/O overhead

- Each output image retains original metadata (timestamps, band
  descriptions)

- Use `recollect = TRUE` to chain with additional VRT operations

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

## See also

`hampel_filter()` for outlier detection,
[`vrt_warp()`](https://permian-global-research.github.io/vrtility/reference/vrt_warp.md)
to prepare aligned collections,
[`vrt_derived_block()`](https://permian-global-research.github.io/vrtility/reference/vrt_derived_block.md)
for band math before filtering

## Examples

``` r
if (FALSE) { # interactive()
#  Set up asynchronous workers to parallelise vrt_collect and vrt_set_maskfun
# number of items:
}
```
