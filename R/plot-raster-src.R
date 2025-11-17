#' plot a raster file or `vrt_x` object
#' @description A wrapper around `gdalraster::plot_raster` that simplifies the
#' process of plotting a raster file.
#' @param x A path to a raster file
#' @param bands a numeric vector of band numbers to plot must be of length
#' 1 or 3.
#' @param col a character vector of colors to use when plotting a single band.
#' for example when sea presents as very dark.
#' @param title a character string indicating what to use as the title of the
#' plot. One of "description", "dttm", or "none". If "description" the band
#' description (name) is used, if "dttm" the datetime of the block is used, if
#' "none" no title is used. Ignored if main is provided.
#' @param rgb_trans a character string indicating the type of RGB transformation
#' to apply when plotting 3 bands. One of "linear", "gamma", "hist", or
#' "hist_all". See details for more information.
#' @rdname plot_raster
#' @details When plotting 3 bands, the `rgb_trans` parameter controls the type
#' of transformation applied to the RGB values before plotting. The options are:
#' - "linear": No transformation is applied (default).
#' - "gamma": A gamma correction is applied to enhance mid-tone contrast.
#' - "hist": Histogram equalization is applied to each band individually to
#'   enhance contrast.
#' - "hist_all": Histogram equalization is applied across all bands to enhance
#'   overall contrast.
#' @examples
#' s2_imgs <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' plot_raster_src(
#'   s2_imgs[3],
#'   bands = c(3, 2, 1),
#' )
#' @export
plot_raster_src <- function(
  x,
  bands = 1,
  col = grDevices::hcl.colors(10, "Viridis"),
  title = c("description", "dttm", "none"),
  rgb_trans = c("linear", "gamma", "hist", "hist_all"),
  col_tbl = NULL,
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
  digits = NULL,
  na_col = "#95acbe",
  ...
) {
  if (!is.null(rgb_trans)) {
    rgb_trans <- rlang::arg_match(rgb_trans)
  }

  title <- rlang::arg_match(title)
  dpi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  dev_inches <- graphics::par("din") # Returns c(width, height) in inches
  dev_size <- dev_inches * dpi

  ds <- methods::new(gdalraster::GDALRaster, x)
  on.exit(ds$close(), add = TRUE)
  rxs <- ds$getRasterXSize()
  rys <- ds$getRasterYSize()

  # get approx scaling factor for plotting  - we dont need all the data.
  scale_factor <- pmax(rxs / dev_size[1], rys / dev_size[2])

  rior_or <- gdalraster::get_config_option("GDAL_RASTERIO_RESAMPLING")
  gdalraster::set_config_option("GDAL_RASTERIO_RESAMPLING", "NEAREST")
  on.exit(
    gdalraster::set_config_option("GDAL_RASTERIO_RESAMPLING", rior_or),
    add = TRUE
  )

  # Determine rgb transformation function gdalraster already applies the linear transform
  if (is.null(minmax_def) && length(bands) == 3 && !is.null(rgb_trans)) {
    if (is.null(minmax_pct_cut)) {
      minmax_pct_cut <- c(0.01, 98)
    }
    trans_fn <- switch(
      rgb_trans,
      linear = NULL,
      gamma = gamma_trans,
      hist = histeq_trans,
      hist_all = hist_all_trans
    )
  } else {
    trans_fn <- NULL
  }

  # Determine main title
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
      ceiling(rxs / scale_factor)
    ),
    ysize = pmin(
      rys,
      ceiling(rys / scale_factor)
    ),
    col = col,
    col_tbl = col_tbl,
    maxColorValue = 1,
    pixel_fn = trans_fn,
    normalize = TRUE,
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
}


#' @param config_options A named character vector of gdal config options to set
#' before attempting to read the VRT.
#' @export
#' @rdname plot_raster
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
