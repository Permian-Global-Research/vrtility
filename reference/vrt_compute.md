# Generate a composite raster from (virtual) raster sources.

Generate a composite raster from (virtual) raster sources.

## Usage

``` r
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs,
  te,
  tr,
  resampling,
  engine,
  warp_options,
  creation_options,
  config_options,
  nsplits,
  add_cl_arg,
  quiet,
  apply_scale,
  dst_nodata,
  recollect
)

# S3 method for class 'vrt_block'
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  resampling = c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_options(),
  nsplits = NULL,
  add_cl_arg = NULL,
  quiet = TRUE,
  apply_scale = TRUE,
  dst_nodata = NULL,
  recollect = FALSE
)

# S3 method for class 'vrt_stack_warped'
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  resampling = c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_options(),
  nsplits = NULL,
  add_cl_arg = NULL,
  quiet = TRUE,
  apply_scale = TRUE,
  dst_nodata = NULL,
  recollect = FALSE
)

# S3 method for class 'vrt_stack'
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs,
  te,
  tr,
  resampling = c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_options(),
  nsplits = NULL,
  add_cl_arg = NULL,
  quiet = TRUE,
  apply_scale = TRUE,
  dst_nodata = NULL,
  recollect = FALSE
)

# S3 method for class 'vrt_collection_warped'
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs = x$srs,
  te = x$bbox,
  tr = x$res,
  resampling = c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_options(),
  nsplits = NULL,
  add_cl_arg = NULL,
  quiet = TRUE,
  apply_scale = TRUE,
  dst_nodata = NULL,
  recollect = FALSE
)

# S3 method for class 'vrt_collection'
vrt_compute(
  x,
  outfile = fs::file_temp(ext = "tif"),
  t_srs,
  te,
  tr,
  resampling = c("near", "bilinear", "cubic", "cubicspline", "lanczos", "average", "rms",
    "mode", "max", "min", "med", "q1", "q3", "sum"),
  engine = c("warp", "gdalraster", "translate"),
  warp_options = gdalwarp_options(),
  creation_options = gdal_creation_options(),
  config_options = gdal_config_options(),
  nsplits = NULL,
  add_cl_arg = NULL,
  quiet = TRUE,
  apply_scale = TRUE,
  dst_nodata = NULL,
  recollect = FALSE
)
```

## Arguments

- x:

  A vrt_block, vrt_stack, or vrt_collection object

- outfile:

  A character string of the output file path

- t_srs:

  A character string of the target SRS

- te:

  A numeric vector of the target extent in the form c(xmin, ymin, xmax,
  ymax) and must be the same SRS as in `t_srs`.

- tr:

  A numeric vector of the target resolution in the form c(xres, yres)

- resampling:

  A character vector of the resampling method to be used. see details.

- engine:

  A character vector of the engine to use for processing the raster
  data. See details.

- warp_options:

  A character vector of options to pass to gdalwarp

- creation_options:

  A character vector of options to pass to the the gdal "engine".

- config_options:

  A character vector of options to set in the GDAL environment

- nsplits:

  An integer of the number of splits to use when using the gdalraster
  engine.

- add_cl_arg:

  A character vector of additional command line arguments that are not
  captured in
  [`gdalwarp_options()`](https://permian-global-research.github.io/vrtility/reference/gdal_options.md) -
  these are not checked for validity.

- quiet:

  A logical indicating whether to suppress output

- apply_scale:

  A logical indicating whether to apply scale values existing the in
  file metadata.

- dst_nodata:

  A numeric value of the nodata value to use for the output raster. If
  NULL, gdal will decide. This is usually only required if you are
  retaining a band which contains a different nodata value to others.

- recollect:

  A logical indicating whether to return the output as a vrt_block or
  vrt_collection object. default is FALSE and the output is a character
  string of the output file path.

## Value

A character string of the path to the output raster or, if recollect is
TRUE, a vrt_block or vrt_collection object.

## Details

The `resampling` default is "near", which should be chosen in vrt_warp
has already been used but "bilinear" may be prefereable where the input
data is has not yet been virtually aligned/resampled.

The choice of `engine` will depend on the nature of the computation
being carried out. In the majority of cases warping is preferred,
especically when we are not processing the entirity of the input dataset
(as is usually the case when working with online data sources).

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
ex_collect <- vrt_collect(s2files)

t_block <- ex_collect[[1]][[1]]

# export each file with mask.
coll_masked <- ex_collect |>
  vrt_set_maskfun(
    mask_band = "SCL",
    mask_values = c(0, 1, 2, 3, 8, 9, 10, 11)
  ) |>
  vrt_warp(
    t_srs = t_block$srs,
    te = t_block$bbox,
    tr = t_block$res
  )

# save each file with masked values.
masked_files <- vrt_compute(
  coll_masked,
  outfile = fs::file_temp(ext = "tif")
)

withr::with_par(
  list(mfrow = c(2, 2)),
  purrr::walk(
    masked_files[2:5],
    ~ plot_raster_src(.x, bands = c(3, 2, 1))
  )
)


basic_mosaic <-
  vrt_stack(coll_masked) |>
  vrt_compute(
    outfile = fs::file_temp(ext = "tif")
  )

# images laid one on top of the other
plot_raster_src(
  basic_mosaic,
  c(3, 2, 1)
)


# now median composite with pixelfunction
med_composite <-
  vrt_stack(coll_masked) |>
  vrt_set_py_pixelfun(pixfun = median_numpy()) |>
  vrt_compute(
    outfile = fs::file_temp(ext = "tif")
  )

plot_raster_src(
  med_composite,
  c(3, 2, 1)
)
```
