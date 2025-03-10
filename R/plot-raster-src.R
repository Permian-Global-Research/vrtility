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
  pal = grDevices::hcl.colors(10, "Viridis"),
  nbands = length(bands),
  col_tbl = NULL,
  maxColorValue = 1,
  normalize = TRUE,
  minmax_def = NULL,
  minmax_pct_cut = NULL,
  col_map_fn = if (nbands == 1) scales::colour_ramp(pal, alpha = FALSE) else
    NULL,
  xlim = NULL,
  ylim = NULL,
  interpolate = TRUE,
  asp = 1,
  axes = TRUE,
  main = "",
  xlab = "",
  ylab = "",
  xaxs = "r",
  yaxs = "r",
  legend = if (nbands == 1) TRUE else FALSE,
  digits = 2,
  na_col = rgb(0, 0, 0, 0),
  ...
) {
  dpi <- dev.size("px")[1] / dev.size("in")[1]
  dev_inches <- graphics::par("din") # Returns c(width, height) in inches
  dev_size <- dev_inches * dpi
  target_divisor <- dev_size[1] * 1.5

  ds <- new(gdalraster::GDALRaster, x)
  on.exit(if (ds$isOpen()) ds$close())

  rxs <- ds$getRasterXSize()
  rys <- ds$getRasterYSize()

  r <- gdalraster::read_ds(
    ds,
    bands = bands,
    out_xsize = pmin(
      rxs,
      ceiling(
        rxs /
          ceiling(rxs / target_divisor)
      )
    ),
    out_ysize = pmin(
      rys,
      ceiling(
        rys /
          ceiling(rys / target_divisor)
      )
    )
  )

  if (is.null(minmax_def) && nbands == 3) {
    if (is.null(minmax_pct_cut)) {
      mm <- purrr::map(
        bands,
        ~ ds$getMinMax(.x, approx_ok = TRUE)
      ) |>
        purrr::reduce(~ c(min(.x[1], .y[1]), max(.x[2], .y[2])))
      minmax_def <- rep(mm, each = nbands) * (1 / 2.2)
    } else {
      mm <- stats::quantile(
        r,
        probs = c(minmax_pct_cut[1] / 100, minmax_pct_cut[2] / 100),
        na.rm = TRUE,
        names = FALSE
      )
      minmax_def <- rep(mm, each = nbands)
    }
  }

  ds$close()

  gdalraster::plot_raster(
    r,
    nbands = nbands,
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
