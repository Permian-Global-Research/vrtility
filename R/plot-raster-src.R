#' plot a raster file
#' @description A wrapper around `gdalraster::plot_raster` that simplifies the
#' process of plotting a raster file.
#' @param x A path to a raster file
#' @param bands a numeric vector of band numbers to plot must be of length
#' 1 or 3.
#' @param pal a character vector of colors to use when plotting a single band.
#' @inheritParams gdalraster::plot_raster
#' @export
plot_raster_src <- function(
  x,
  bands = 1,
  pal = grDevices::hcl.colors(10, "viridis"),
  xsize = NULL,
  ysize = NULL,
  nbands = length(bands),
  max_pixels = 2.5e+07,
  col_tbl = NULL,
  maxColorValue = 1,
  normalize = TRUE,
  minmax_def = NULL,
  minmax_pct_cut = if (length(bands) == 3) c(1, 97) else NULL,
  col_map_fn = if (nbands == 1) scales::colour_ramp(pal, alpha = FALSE) else
    NULL,
  xlim = NULL,
  ylim = NULL,
  interpolate = TRUE,
  asp = 1,
  axes = TRUE,
  main = "",
  xlab = "x",
  ylab = "y",
  xaxs = "i",
  yaxs = "i",
  legend = if (nbands == 1) TRUE else FALSE,
  digits = 2,
  na_col = rgb(0, 0, 0, 0),
  ...
) {
  ds <- new(gdalraster::GDALRaster, x)

  r <- gdalraster::read_ds(ds, bands = bands)

  ds$close()

  gdalraster::plot_raster(
    r,
    xsize = xsize,
    ysize = ysize,
    nbands = nbands,
    max_pixels = max_pixels,
    col_tbl = col_tbl,
    maxColorValue = maxColorValue,
    normalize = normalize,
    minmax_def = minmax_def,
    minmax_pct_cut = minmax_pct_cut,
    col_map_fn = col_map_fn,
    xlim = xlim,
    ylim = ylim,
    interpolate = interpolate,
    asp = asp,
    axes = axes,
    main = main,
    xlab = xlab,
    ylab = ylab,
    xaxs = xaxs,
    yaxs = yaxs,
    legend = legend,
    digits = digits,
    na_col = na_col,
    ...
  )
}
