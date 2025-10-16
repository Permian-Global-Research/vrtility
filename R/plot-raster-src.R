#' plot a raster file or `vrt_x` object
#' @description A wrapper around `gdalraster::plot_raster` that simplifies the
#' process of plotting a raster file.
#' @param x A path to a raster file
#' @param bands a numeric vector of band numbers to plot must be of length
#' 1 or 3.
#' @param pal a character vector of colors to use when plotting a single band.
#' for example when sea presents as very dark.
#' @param title a character string indicating what to use as the title of the
#' plot. One of "description", "dttm", or "none". If "description" the band
#' description (name) is used, if "dttm" the datetime of the block is used, if
#' "none" no title is used. Ignored if main is provided.
#' @inheritParams gdalraster::plot_raster
#' @rdname plot_raster
#' @export
plot_raster_src <- function(
  x,
  bands = 1,
  col = grDevices::hcl.colors(10, "Viridis"),
  title = c("description", "dttm", "none"),
  rgb_trans = c("linear", "gamma", "hist", "hist_all", "none"),
  col_tbl = NULL,
  maxColorValue = 1,
  normalize = TRUE,
  minmax_def = NULL,
  minmax_pct_cut = NULL,
  xlim = NULL,
  ylim = NULL,
  interpolate = TRUE,
  axes = TRUE,
  main = "",
  xlab = "",
  ylab = "",
  legend = if (length(bands) == 1) TRUE else FALSE,
  digits = 2,
  na_col = grDevices::rgb(0, 0, 0, 0),
  ...
) {
  rgb_trans <- rlang::arg_match(rgb_trans)
  title <- rlang::arg_match(title)
  dpi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  dpi <- 500 / 10
  # dev_inches <- graphics::par("din") # Returns c(width, height) in inches
  dev_inches <- c(10, 8)
  dev_size <- dev_inches * dpi
  target_divisor <- dev_size[1] * 2

  ds <- methods::new(gdalraster::GDALRaster, x)
  on.exit(ds$close(), add = TRUE)
  rxs <- ds$getRasterXSize()
  rys <- ds$getRasterYSize()

  rior_or <- gdalraster::get_config_option("GDAL_RASTERIO_RESAMPLING")
  gdalraster::set_config_option("GDAL_RASTERIO_RESAMPLING", "BILINEAR")
  on.exit(
    gdalraster::set_config_option("GDAL_RASTERIO_RESAMPLING", rior_or),
    add = TRUE
  )

  if (is.null(minmax_def) && length(bands) == 3) {
    # if (is.null(minmax_pct_cut)) {
    #   # minmax_pct_cut <- c(0.02, 0.98)
    # }
    trans_fn <- switch(
      rgb_trans,
      linear = linear_trans,
      gamma = gamma_trans,
      hist = histeq_trans,
      hist_all = hist_all_trans,
      none = NULL
    )
    # r <- trans_fn(r, minmax_pct_cut[1], minmax_pct_cut[2])
    # minmax_pct_cut <- NULL
  } else {
    trans_fn <- NULL
  }

  if (nchar(main) == 0 && length(bands) == 1) {
    if (title == "description") {
      main <- ds$getDescription(bands)
    } else if (title == "dttm") {
      main <- ds$getMetadataItem(0, "datetime", "")
    }
  } else if (nchar(main) == 0 && length(bands) == 3) {
    if (title != "none") {
      main <- ds$getMetadataItem(0, "datetime", "")
    }
  }

  plot(
    ds,
    bands = bands,
    xsize = pmin(
      rxs,
      ceiling(
        rxs /
          ceiling(rxs / target_divisor)
      )
    ),
    ysize = pmin(
      rys,
      ceiling(
        rys /
          ceiling(rys / target_divisor)
      )
    ),
    col = col,
    col_tbl = col_tbl,
    maxColorValue = maxColorValue,
    pixel_fn = trans_fn,
    normalize = normalize,
    minmax_def = minmax_def,
    minmax_pct_cut = minmax_pct_cut,
    xlim = xlim,
    ylim = ylim,
    interpolate = interpolate,
    axes = axes,
    main = main,
    xlab = xlab,
    ylab = ylab,
    legend = legend,
    digits = digits,
    na_col = na_col,
    ...
  )

  # par_orig <- graphics::par(no.readonly = TRUE)
  # mfg <- graphics::par("mfg")

  # on.exit(
  #   {
  #     if (mfg[1] == mfg[3] && mfg[2] == mfg[4]) {
  #       graphics::par(par_orig)
  #     }
  #   },
  #   add = TRUE
  # )

  invisible()
}


#' @param config_options A named character vector of gdal config options to set
#' before attempting to read the VRT.
#' @export
#' @rdname plot_raster
#' @inheritParams plot_raster_src
plot.vrt_block <- function(
  x,
  ...,
  config_options = gdal_config_opts()
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
  config_options = gdal_config_opts()
) {
  if (!inherits(x, "vrt_stack_warped")) {
    cli::cli_inform(
      c(
        "i" = "You are plotting a non-warped vrt_stack - this is probably okay",
        " " = "But, as no extent parameters are set, the plot may not be as
      expected."
      )
    )
  }
  NextMethod()
}


#' @export
#' @rdname plot_raster
plot.vrt_stack_warped <- function(
  x,
  ...,
  config_options = gdal_config_opts()
) {
  NextMethod()
}

#' @param item The numeric index of the item, in the vrt_collection, to plot
#' @export
#' @rdname plot_raster
plot.vrt_collection <- function(
  x,
  item,
  ...,
  config_options = gdal_config_opts()
) {
  x <- x[[1]][[item]]
  plot(x, ..., config_options = config_options)
}
