# Helper functions for plot.Rcpp_GDALRaster

#' Validate and setup plot parameters
#' @noRd
.validate_plot_params <- function(bands, max_pixels, col) {
  # Device capabilities check
  if (isTRUE((grDevices::dev.capabilities()$rasterImage == "no"))) {
    message("device does not support 'rasterImage()'")
    return(list(valid = FALSE))
  }

  # Bands validation
  if (!is.null(bands)) {
    if (!(length(bands) %in% c(1, 3))) {
      stop("bands argument must be of length 1 or 3", call. = FALSE)
    }
  } else {
    message("bands argument is NULL, defaulting to band 1")
    bands <- 1
  }

  # Max pixels handling
  if (is.null(max_pixels)) {
    max_pixels <- Inf
  }

  # Color validation and setup
  if (!is.null(col)) {
    if (!is.character(col)) {
      stop(
        "'col' must be a character string see grDevices::col2rgb()",
        call. = FALSE
      )
    }
  } else {
    col <- grDevices::hcl.colors(10, "Viridis")
  }

  # Create color mapping function
  if (length(bands) == 1) {
    col_map_fn <- scales::colour_ramp(col, alpha = FALSE)
  } else {
    col_map_fn <- NULL
  }

  return(list(
    valid = TRUE,
    bands = bands,
    max_pixels = max_pixels,
    col = col,
    col_map_fn = col_map_fn
  ))
}

#' Setup plot dimensions and reading parameters
#' @noRd
.setup_plot_dimensions <- function(data, xsize, ysize, max_pixels) {
  dm <- as.numeric(data$dim())

  if (is.null(xsize)) {
    xsize <- dm[1]
  } else {
    xsize <- trunc(xsize)
  }
  if (is.null(ysize)) {
    ysize <- dm[2]
  } else {
    ysize <- trunc(ysize)
  }

  # Apply max_pixels constraint
  if ((xsize * ysize) > max_pixels) {
    ovr_ratio <- sqrt((xsize * ysize) / max_pixels)
    xsize <- trunc(xsize / ovr_ratio)
    ysize <- trunc(ysize / ovr_ratio)
    warning(
      "'xsize * ysize' exceeds 'max_pixels', downsampling applied",
      call. = FALSE
    )
  }

  return(list(
    dm = dm,
    xsize = xsize,
    ysize = ysize
  ))
}

#' Read and process raster data
#' @noRd
.read_and_process_data <- function(
  data,
  bands,
  dimensions,
  scale_values,
  pixel_fn
) {
  dm <- dimensions$dm
  xsize <- dimensions$xsize
  ysize <- dimensions$ysize

  # Check if byte raster
  is_byte_raster <- TRUE
  for (b in bands) {
    if (data$getDataTypeName(b) != "Byte") {
      is_byte_raster <- FALSE
    }
  }

  # Read the data
  data_in <- gdalraster::read_ds(
    data,
    bands = bands,
    xoff = 0,
    yoff = 0,
    xsize = dm[1],
    ysize = dm[2],
    out_xsize = xsize,
    out_ysize = ysize
  )

  data_in[is.infinite(data_in)] <- NA

  nbands <- length(bands)

  # Apply scaling if needed
  if (nbands == 1 && isTRUE(scale_values)) {
    scale_val <- data$getScale(bands[1])
    if (!is.na(scale_val) && scale_val != 1) {
      data_in <- data_in * scale_val
    }
    offset_val <- data$getOffset(bands[1])
    if (!is.na(offset_val) && offset_val != 0) {
      data_in <- data_in + offset_val
    }
  }

  # Handle raw data
  if (typeof(data_in) == "raw") {
    data_in <- as.integer(data_in)
  }

  # Apply pixel function if provided
  if (!is.null(pixel_fn)) {
    if (!is.function(pixel_fn)) {
      stop("'pixel_fn' must be a function", call. = FALSE)
    }
    data_in <- pixel_fn(data_in)
  }

  # Check for complex data
  if (typeof(data_in) == "complex") {
    stop("specify 'pixel_fn' when plotting complex data types", call. = FALSE)
  }

  return(list(
    data_in = data_in,
    nbands = nbands,
    is_byte_raster = is_byte_raster
  ))
}

#' Setup coordinate system and limits
#' @noRd
.setup_coordinates <- function(data, dimensions, xlim, ylim) {
  dm <- dimensions$dm
  gt <- data$getGeoTransform()
  south_up <- FALSE

  if (is.null(xlim)) {
    xlim <- c(gt[1], gt[1] + gt[2] * dm[1])
  }
  if (is.null(ylim)) {
    ylim <- c(gt[4] + gt[6] * dm[2], gt[4])
    if (gt[6] > 0) {
      south_up <- TRUE
      ylim <- rev(ylim)
    }
  }

  return(list(
    xlim = xlim,
    ylim = ylim,
    south_up = south_up
  ))
}

#' Create discrete-aware color mapping
#' @noRd
.create_color_mapping <- function(
  data_in,
  nbands,
  col_tbl,
  col_map_fn,
  normalize,
  nodata_value = NULL
) {
  final_col_map_fn <- col_map_fn
  use_normalization <- normalize

  if (nbands == 1 && is.null(col_tbl) && !is.null(col_map_fn)) {
    # Filter out NoData values for discrete detection
    if (!is.null(nodata_value)) {
      unique_vals <- unique(data_in[!is.na(data_in) & data_in != nodata_value])
    } else {
      unique_vals <- unique(data_in[!is.na(data_in)])
    }
    n_unique_vals <- length(unique_vals)

    if (n_unique_vals < 12) {
      # This is discrete data
      use_normalization <- FALSE # Turn off normalization for discrete data

      # For discrete data, create a custom color mapping function
      unique_vals_sorted <- sort(unique_vals)

      # Get distinct colors using evenly spaced positions
      if (n_unique_vals > 1) {
        color_positions <- seq(0, 1, length.out = n_unique_vals)
      } else {
        color_positions <- 0.5
      }
      discrete_colors <- col_map_fn(color_positions)

      # Create a function that maps actual values to discrete colors
      # Using match() is much faster than looping
      final_col_map_fn <- function(x) {
        # Use findInterval or match for O(n) instead of O(n*m) complexity
        indices <- match(x, unique_vals_sorted)
        result <- discrete_colors[indices]
        # Handle any unmatched values (shouldn't happen, but safety)
        result[is.na(result)] <- discrete_colors[1]
        return(result)
      }
    }
  }

  return(list(
    col_map_fn = final_col_map_fn,
    use_normalization = use_normalization
  ))
}

#' Draw axes and labels for raster plot
#' @noRd
.draw_axes_and_labels <- function(xlim, ylim, main, xlab, ylab, axes) {
  if (axes) {
    # Always use consistent axis behavior regardless of legend/band count
    plot_center_x <- mean(xlim)

    # Position titles relative to canvas edges but with more spacing
    graphics::mtext(
      main,
      side = 3,
      line = 0.5,
      at = plot_center_x,
      cex = 1.5,
      font = 2
    )
    graphics::mtext(xlab, side = 1, line = 2.5, at = plot_center_x, cex = 1.3)
    graphics::mtext(ylab, side = 2, line = 3.0, cex = 1.3)

    # Consistent axis behavior - always use pretty ticks within xlim range
    x_ticks <- pretty(xlim, n = 5)
    x_ticks <- x_ticks[x_ticks >= xlim[1] & x_ticks <= xlim[2]]
    graphics::axis(
      1,
      line = 0,
      at = x_ticks,
      labels = x_ticks,
      pos = ylim[1],
      lwd = 0,
      lwd.ticks = 1,
      cex.axis = 1.2
    )

    y_ticks <- pretty(ylim, n = 5)
    y_ticks <- y_ticks[y_ticks >= ylim[1] & y_ticks <= ylim[2]]
    graphics::axis(
      2,
      line = 0,
      at = y_ticks,
      labels = y_ticks,
      pos = xlim[1],
      lwd = 0,
      lwd.ticks = 1,
      cex.axis = 1.2
    )

    # Add lines to complete the box around plot extent
    graphics::segments(xlim[1], ylim[2], xlim[2], ylim[2]) # top line
    graphics::segments(xlim[2], ylim[1], xlim[2], ylim[2]) # right line
    graphics::segments(xlim[1], ylim[1], xlim[2], ylim[1]) # bottom line
    graphics::segments(xlim[1], ylim[1], xlim[1], ylim[2]) # left line
  } else {
    # No axes case - still position title consistently
    plot_center_x <- mean(xlim)
    graphics::mtext(
      main,
      side = 3,
      line = 0.5,
      at = plot_center_x,
      cex = 1.8,
      font = 2
    )
  }
}

#' Create legend for raster plot
#' @noRd
.create_legend <- function(
  data_in,
  col_map_fn,
  col_tbl,
  normalize,
  minmax_def,
  minmax_pct_cut,
  maxColorValue,
  na_col,
  nodata_value = NULL
) {
  # Filter out NoData values for calculations
  if (!is.null(nodata_value)) {
    valid_data <- data_in[!is.na(data_in) & data_in != nodata_value]
  } else {
    valid_data <- data_in[!is.na(data_in)]
  }

  # Handle edge case of no valid data early to avoid warnings
  if (length(valid_data) == 0) {
    # Create a simple single-color legend for no-data case
    leg_colors <- rep("#CCCCCC", 256) # Gray color for no data
    leg_img <- grDevices::as.raster(matrix(leg_colors, ncol = 1))
    return(list(
      img = leg_img,
      mm = c(0, 1), # Default range
      unique_values = NULL,
      is_discrete = TRUE,
      n_unique = 0
    ))
  }

  # Calculate data range
  mm <- NULL
  if (!is.null(minmax_def)) {
    mm <- minmax_def
  } else if (!is.null(minmax_pct_cut)) {
    mm <- stats::quantile(
      valid_data,
      probs = c(minmax_pct_cut[1] / 100, minmax_pct_cut[2] / 100),
      na.rm = TRUE,
      names = FALSE
    )
  } else {
    mm <- c(min(valid_data, na.rm = TRUE), max(valid_data, na.rm = TRUE))
  }

  # Check for discrete vs continuous legend
  unique_values <- unique(valid_data)
  n_unique <- length(unique_values)
  is_discrete <- n_unique < 12

  # Create legend image
  if (is.null(col_tbl)) {
    if (is_discrete) {
      # Discrete legend: remap values to sequential indices for distinct colors
      unique_values <- sort(unique_values)

      # For discrete data, use sequential positions to get distinct colors
      if (n_unique > 1) {
        normalized_values <- seq(0, 1, length.out = n_unique)
      } else {
        normalized_values <- 0.5
      }

      # Get distinct colors for each unique value
      colors_for_values <- col_map_fn(normalized_values)

      # Create distinct color segments
      legend_height <- 256
      pixels_per_value <- floor(legend_height / n_unique)
      leg_colors <- character(legend_height)
      for (i in 1:n_unique) {
        start_idx <- (i - 1) * pixels_per_value + 1
        end_idx <- min(i * pixels_per_value, legend_height)
        leg_colors[start_idx:end_idx] <- colors_for_values[i]
      }
      if (any(leg_colors == "")) {
        leg_colors[leg_colors == ""] <- colors_for_values[n_unique]
      }
      leg_colors <- rev(leg_colors)
      leg_img <- grDevices::as.raster(matrix(leg_colors, ncol = 1))
    } else {
      # Continuous legend
      leg_data <- seq(mm[1], mm[2], length.out = 256)
      if (normalize) {
        leg_data <- gdalraster:::.normalize(leg_data, mm)
      }
      leg_data <- sort(leg_data, decreasing = TRUE)
      leg_data <- col_map_fn(leg_data)
      leg_img <- grDevices::as.raster(matrix(leg_data, ncol = 1))
    }
  } else {
    leg_data <- sort(seq(mm[1], mm[2], by = 1), decreasing = TRUE)
    leg_data <- array(leg_data, dim = c(1, length(leg_data), 1))
    leg_img <- gdalraster:::.as_raster(
      leg_data,
      col_tbl = col_tbl,
      maxColorValue = maxColorValue,
      na_col = na_col
    )
  }

  return(list(
    img = leg_img,
    mm = mm,
    unique_values = if (is_discrete) unique_values else NULL,
    is_discrete = is_discrete,
    n_unique = n_unique
  ))
}

#' Automatically determine appropriate number of digits for legend labels
#' @param values Numeric vector of values to format
#' @return Integer number of decimal places
#' @noRd
.auto_determine_digits <- function(values) {
  # Remove NA and infinite values
  orig_length <- length(values)
  clean_values <- values[is.finite(values)]

  if (length(clean_values) == 0) {
    return(0)
  }

  # If all values are integers, use 0 digits
  if (all(clean_values == floor(clean_values)) && orig_length > 2) {
    return(0)
  }

  # Calculate the range and magnitude of values
  value_range <- diff(range(clean_values, na.rm = TRUE))
  max_magnitude <- max(abs(clean_values), na.rm = TRUE)

  # For large magnitude values, use fewer digits
  if (max_magnitude >= 1000) {
    return(0) # Large numbers, no decimals needed
  } else if (max_magnitude >= 100) {
    return(1) # Hundreds, 1 decimal
  } else if (max_magnitude >= 10) {
    return(1) # Tens, 1 decimal
  } else if (value_range >= 1) {
    return(2) # Range spans units, 2 decimals
  } else if (value_range >= 0.1) {
    return(2) # Range spans tenths, 2 decimals
  } else if (value_range >= 0.01) {
    return(3) # Range spans hundredths, 3 decimals
  } else {
    # For very small ranges, use scientific notation logic
    # Find the smallest non-zero difference between values
    if (length(clean_values) > 1) {
      sorted_vals <- sort(unique(clean_values))
      if (length(sorted_vals) > 1) {
        # Need at least 2 unique values for diff
        min_diff <- min(diff(sorted_vals))
        if (min_diff > 0) {
          # Number of digits needed to represent the smallest difference
          digits_needed <- max(0, ceiling(-log10(min_diff)) + 1)
          return(min(digits_needed, 6)) # Cap at 6 digits
        }
      }
    }
    return(4) # Default for very small values
  }
}

#' Draw legend on plot
#' @noRd
.draw_legend <- function(legend_data, xlim, ylim, digits = NULL) {
  # Calculate legend position
  legend_x_start <- xlim[2] + 0.02 * diff(xlim)
  legend_x_end <- xlim[2] + 0.07 * diff(xlim)
  y_range <- diff(ylim)
  legend_y_start <- ylim[1] + 0.1 * y_range
  legend_y_end <- ylim[2] - 0.1 * y_range

  # Draw the legend raster
  graphics::rasterImage(
    legend_data$img,
    legend_x_start,
    legend_y_start,
    legend_x_end,
    legend_y_end
  )

  # Add outline
  graphics::rect(
    legend_x_start,
    legend_y_start,
    legend_x_end,
    legend_y_end,
    border = "black",
    lwd = 1
  )

  # Add labels
  if (legend_data$is_discrete) {
    unique_values <- legend_data$unique_values

    # Handle case where there are no unique values (all NA)
    if (is.null(unique_values) || length(unique_values) == 0) {
      leg_lab <- "No Data"
      text_y <- (legend_y_start + legend_y_end) / 2
    } else {
      if (is(unique_values, "integer")) {
        leg_lab <- formatC(rev(unique_values), format = "d")
      } else {
        # Auto-determine digits for discrete values if not specified
        if (is.null(digits)) {
          # For discrete data, use minimal digits needed to distinguish values
          digits <- .auto_determine_digits(unique_values)
        }
        leg_lab <- formatC(rev(unique_values), format = "f", digits = digits)
      }
      legend_height_plot <- legend_y_end - legend_y_start
      block_height <- legend_height_plot / legend_data$n_unique
      text_y <- seq(
        legend_y_end - block_height / 2,
        legend_y_start + block_height / 2,
        length.out = legend_data$n_unique
      )
    }
  } else {
    mm <- legend_data$mm
    # Auto-determine digits for continuous data if not specified
    if (is.null(digits)) {
      digits <- .auto_determine_digits(mm)
    }
    leg_lab <- formatC(
      seq(mm[1], mm[2], length.out = 5),
      format = "f",
      digits = digits
    )
    text_y <- seq(legend_y_start, legend_y_end, length.out = 5)
  }

  # Position text and ticks
  text_x <- xlim[2] + 0.09 * diff(xlim)
  tick_length <- 0.01 * diff(xlim)
  graphics::segments(
    legend_x_end,
    text_y,
    legend_x_end + tick_length,
    text_y,
    lwd = 1
  )
  graphics::text(text_x, text_y, labels = leg_lab, adj = c(0, 0.5), cex = 1.2)
}

#' @param x An object of class \code{Rcpp_GDALRaster} see
#' \code{\link[gdalraster]{GDALRaster}}
#' @param bands Integer vector of length 1 or 3. Band number(s) to plot.
#' For grayscale plots use a single band, for RGB use three bands.
#' @param xsize Integer. Desired width of the plotted raster in pixels.
#' If NULL (default), dimension is rescaled in line with the device size.
#' @param ysize Integer. Desired height of the plotted raster in pixels.
#' If NULL (default), dimension is rescaled in line with the device size.
#' @param max_pixels Numeric. Maximum number of pixels to read. If
#' \code{xsize * ysize} exceeds this value, the raster will be downsampled.
#' @param scale_values Logical. Whether to apply scale and offset values
#' from the raster metadata.
#' @param col_tbl A data frame with the columns:
#' "VALUE", "RED", "GREEN", "BLUE".
#' @param maxColorValue Numeric. Maximum color value when using a color table.
#' @param normalize Logical. Whether to normalize pixel values to \[0,1\] range
#' before color mapping. Automatically disabled for discrete data.
#' @param minmax_def Numeric vector of min/max values for color scaling.
#' For RGB plots, should be length 6: c(min_r, min_g, min_b, max_r, max_g, max_b).
#' @param minmax_pct_cut Numeric vector of length 2. Percentile cutoff values
#' for color scaling (e.g., c(2, 98) for 2nd and 98th percentiles).
#' @param col Colors to interpolate; must be a valid argument to
#' \code{\link[grDevices]{col2rgb}}. Ignored if \code{col_tbl} is provided.
#' @param pixel_fn Function to apply to pixel values before plotting.
#' @param xlim Numeric vector of length 2. X-axis limits in coordinate system units.
#' @param ylim Numeric vector of length 2. Y-axis limits in coordinate system units.
#' @param interpolate Logical. Whether to apply smoothing to the raster image.
#' @param axes Logical. Whether to draw axes and axis labels.
#' @param main Character string. Plot title.
#' @param xlab Character string. X-axis label.
#' @param ylab Character string. Y-axis label.
#' @param legend Logical. Whether to draw a color legend. Only supported for
#' single-band plots. Automatically detects discrete vs continuous data.
#' @param digits Integer. Number of decimal places for legend labels. If `NULL`
#' (default), automatically determines appropriate precision based on data range.
#' @param na_col Color for NA/nodata pixels. Default is transparent.
#' @param mar Numeric vector of length 4. Additional margin adjustments
#' to add to the base margins when legend is enabled.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot.window}}.
#' @rdname plot_raster
#' @examples
#' s2_imgs <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
#' ds <- methods::new(gdalraster::GDALRaster, s2_imgs[2])
#'
#' plot(
#'   ds,
#'   bands = 4,
#'   legend = TRUE
#' )
#'
#' ds$close()
#' @export
plot.Rcpp_GDALRaster <- function(
  x,
  bands = 1,
  xsize = NULL,
  ysize = NULL,
  max_pixels = 2.5e+07,
  scale_values = TRUE,
  col_tbl = NULL,
  maxColorValue = 1,
  normalize = TRUE,
  minmax_def = NULL,
  minmax_pct_cut = NULL,
  col = NULL,
  pixel_fn = NULL,
  xlim = NULL,
  ylim = NULL,
  interpolate = TRUE,
  axes = TRUE,
  main = "",
  xlab = "x",
  ylab = "y",
  legend = FALSE,
  digits = NULL,
  na_col = grDevices::rgb(0, 0, 0, 0),
  mar = c(0, 0, 0, 0),
  ...
) {
  # browser()
  # get adjusted raster dims:
  if (is.null(xsize) && is.null(ysize)) {
    dims <- raster_dims_scale_by_device(x)
    xsize <- dims$xsize
    ysize <- dims$ysize
  }

  # 1. Validate inputs and setup basic parameters
  params <- .validate_plot_params(bands, max_pixels, col)
  if (!params$valid) {
    return()
  }

  bands <- params$bands
  col_map_fn <- params$col_map_fn

  # 2. Setup plot dimensions
  dimensions <- .setup_plot_dimensions(x, xsize, ysize, params$max_pixels)

  # 3. Read and process the raster data
  processed_data <- .read_and_process_data(
    x,
    bands,
    dimensions,
    scale_values,
    pixel_fn
  )
  data_in <- processed_data$data_in
  nbands <- processed_data$nbands
  is_byte_raster <- processed_data$is_byte_raster

  # 4. Handle color table setup for single band plots
  if (nbands == 1 && is.null(col_tbl) && is.null(col)) {
    if (
      !is.null(x$getColorTable(band = 1)) &&
        x$getPaletteInterp(band = 1) == "RGB"
    ) {
      col_tbl <- x$getColorTable(band = 1)
      maxColorValue <- 255
    }
  }

  # 5. Setup coordinate system
  coords <- .setup_coordinates(x, dimensions, xlim, ylim)
  xlim <- coords$xlim
  ylim <- coords$ylim
  south_up <- coords$south_up

  # 6. Handle RGB byte raster default normalization
  if (
    nbands == 3 &&
      is_byte_raster &&
      normalize &&
      is.null(minmax_def) &&
      is.null(minmax_pct_cut)
  ) {
    minmax_def <- c(0, 0, 0, 255, 255, 255)
  }

  # 7. Apply south-up transformation if needed
  if (south_up) {
    data_in <- gdalraster:::.flip_vertical(
      data_in,
      dimensions$xsize,
      dimensions$ysize,
      nbands
    )
  }

  # 7.5. Get NoData value for single band rasters
  nodata_value <- NULL
  if (nbands == 1) {
    nodata_value <- x$getNoDataValue(bands[1])
    if (is.na(nodata_value)) {
      nodata_value <- NULL
    }
  }

  # 8. Create discrete-aware color mapping
  color_mapping <- .create_color_mapping(
    data_in,
    nbands,
    col_tbl,
    col_map_fn,
    normalize,
    nodata_value
  )

  # 9. Create the main raster image
  a <- array(data_in, dim = c(dimensions$xsize, dimensions$ysize, nbands))
  r <- gdalraster:::.as_raster(
    a,
    col_tbl = col_tbl,
    maxColorValue = maxColorValue,
    normalize = color_mapping$use_normalization,
    minmax_def = minmax_def,
    minmax_pct_cut = minmax_pct_cut,
    col_map_fn = color_mapping$col_map_fn,
    na_col = na_col
  )

  # 10. Disable legend for RGB plots
  if (legend && nbands != 1) {
    message("legend is not supported for RGB plot")
    legend <- FALSE
  }

  # 11. Setup plot margins
  op_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = op_mar))

  # Adjust margins based on label content
  # Top margin for title
  top_margin <- if (is.null(main) || main == "" || nchar(trimws(main)) == 0) {
    1
  } else {
    3
  }

  # Bottom margin for x-axis label
  bottom_margin <- if (
    is.null(xlab) || xlab == "" || nchar(trimws(xlab)) == 0
  ) {
    2
  } else {
    4
  }

  # Left margin for y-axis label
  left_margin <- if (is.null(ylab) || ylab == "" || nchar(trimws(ylab)) == 0) {
    2
  } else {
    4
  }

  if (legend) {
    base_mar <- c(bottom_margin, left_margin, top_margin, 0.75) + 0.1
    graphics::par(mar = base_mar + mar)
  } else {
    # Set margins even when no legend to control spacing
    base_mar <- c(bottom_margin, left_margin, top_margin, 1) + 0.1
    graphics::par(mar = base_mar + mar)
  }

  # 12. Create plot window
  graphics::plot.new()
  xlim_extended <- if (legend) c(xlim[1], xlim[1] + 1.20 * diff(xlim)) else xlim
  graphics::plot.window(
    xlim = xlim_extended,
    ylim = ylim,
    asp = 1,
    xaxs = "i",
    yaxs = "i",
    ...
  )

  # 13. Draw the main raster image
  graphics::rasterImage(
    r,
    xlim[1],
    ylim[1],
    xlim[2],
    ylim[2],
    interpolate = interpolate
  )

  # 14. Draw axes and labels
  .draw_axes_and_labels(xlim, ylim, main, xlab, ylab, axes)

  # 15. Draw legend if requested
  if (legend) {
    legend_data <- .create_legend(
      data_in,
      col_map_fn,
      col_tbl,
      normalize,
      minmax_def,
      minmax_pct_cut,
      maxColorValue,
      na_col,
      nodata_value
    )
    .draw_legend(legend_data, xlim, ylim, digits)
  }

  invisible()
}


raster_dims_scale_by_device <- function(ds) {
  dpi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  dev_inches <- graphics::par("din") # Returns c(width, height) in inches
  dev_size <- dev_inches * dpi

  rxs <- ds$getRasterXSize()
  rys <- ds$getRasterYSize()

  # get approx scaling factor for plotting  - we dont need all the data.
  scale_factor <- pmax(rxs / dev_size[1], rys / dev_size[2])

  return(list(
    xsize = pmin(
      rxs,
      ceiling(rxs / scale_factor)
    ),
    ysize = pmin(
      rys,
      ceiling(rys / scale_factor)
    )
  ))
}
