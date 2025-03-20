#' plot a raster file or `vrt_x` object
#' @description A wrapper around `gdalraster::plot_raster` that simplifies the
#' process of plotting a raster file.
#' @param x A path to a raster file
#' @param bands a numeric vector of band numbers to plot must be of length
#' 1 or 3.
#' @param pal a character vector of colors to use when plotting a single band.
#' @param adj_low a numeric value to adjust the lower quantile value by. useful
#' for example when sea presents as very dark.
#' @inheritParams gdalraster::plot_raster
#' @rdname plot_raster
#' @export
plot_raster_src <- function(
  x,
  bands = 1,
  pal = grDevices::hcl.colors(10, "Viridis"),
  nbands = length(bands),
  col_tbl = NULL,
  maxColorValue = 1,
  normalize = TRUE,
  adj_low = 0.8,
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
  na_col = grDevices::rgb(0, 0, 0, 0),
  ...
) {
  dpi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  dev_inches <- graphics::par("din") # Returns c(width, height) in inches
  dev_size <- dev_inches * dpi
  target_divisor <- dev_size[1] * 1.5

  ds <- methods::new(gdalraster::GDALRaster, x)
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

  if (is.null(minmax_def) && is.null(minmax_pct_cut) && nbands == 3) {
    mm <- stats::quantile(
      r,
      probs = c(0, 0.98),
      na.rm = TRUE,
      names = FALSE
    )
    mm[1] <- mm[1] * adj_low

    minmax_def <- rep(mm, each = nbands)
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


#' @param config_options A named character vector of gdal config options to set
#' before attempting to read the VRT.
#' @param quiet Logical indicating whether to suppress messages.
#' @export
#' @rdname plot_raster
#' @inheritParams plot_raster_src
plot.vrt_block <- function(
  x,
  ...,
  config_options = gdal_config_opts(),
  quiet = FALSE
) {
  orig_config <- set_gdal_config(config_options)
  on.exit(set_gdal_config(orig_config))
  src <- vrt_save(x)
  compute_with_py_env(plot_raster_src(src, ...))
}

#' @export
#' @rdname plot_raster
plot.vrt_stack <- function(
  x,
  ...,
  config_options = gdal_config_opts(),
  quiet = FALSE
) {
  if (!inherits(x, "vrt_stack_warped")) {
    if (!quiet) {
      cli::cli_inform(
        c(
          "i" = "You are plotting a non-warped vrt_stack - this is probably okay",
          " " = "But, as no extent parameters are set, the plot may not be as
      expected."
        )
      )
    }
  }
  NextMethod()
}


#' @export
#' @rdname plot_raster
plot.vrt_stack_warped <- function(
  x,
  ...,
  config_options = gdal_config_opts(),
  quiet = FALSE
) {
  if (!quiet) {
    cli::cli_inform(
      c(
        "i" = "You a plotting a warped raster - this might be okay...",
        " " = "But, If this is taking a long time you are probably better",
        " " = "off saving the file first with `vrt_compute` and then plotting",
        " " = "with `plot_raster_src()`."
      )
    )
  }
  NextMethod()
}

#' @param item The numeric index of the item, in the vrt_collection, to plot
#' @export
#' @rdname plot_raster
plot.vrt_collection <- function(
  x,
  item,
  ...,
  config_options = gdal_config_opts(),
  quiet = FALSE
) {
  x <- x[[1]][[item]]
  plot(x, ..., config_options = config_options)
}
