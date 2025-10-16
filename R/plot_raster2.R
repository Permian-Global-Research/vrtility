#' @param data An object of class \code{Rcpp_GDALRaster} see
#' \code{\link[gdalraster]{GDALRaster}}
#' @param col Colors to interpolate; must be a valid argument to
#' \code{\link[grDevices]{col2rgb}}
#' @rdname plot_raster
#' @export
plot.Rcpp_GDALRaster <- function(
  data,
  xsize = NULL,
  ysize = NULL,
  bands = NULL,
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
  digits = 2,
  na_col = grDevices::rgb(0, 0, 0, 0),
  mar = c(0, 0, 0, 0),
  ...
) {
  if (isTRUE((grDevices::dev.capabilities()$rasterImage == "no"))) {
    message("device does not support 'rasterImage()'")
    return()
  }
  if (!is.null(bands)) {
    if (!(length(bands) %in% c(1, 3))) {
      stop("bands argument must be of length 1 or 3", call. = FALSE)
    }
  } else {
    message("bands argument is NULL, defaulting to band 1")
    bands <- 1
  }
  if (is.null(max_pixels)) {
    max_pixels <- Inf
  }
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

  if (length(bands) == 1) {
    col_map_fn <- scales::colour_ramp(col, alpha = FALSE)
  } else {
    col_map_fn <- NULL
  }

  if (!is.null(pixel_fn)) {
    if (!is.function(pixel_fn)) {
      stop("'pixel_fn' must be a function", call. = FALSE)
    }
  }
  data_in <- NULL
  is_byte_raster <- TRUE
  south_up <- FALSE

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
  if ((xsize * ysize) > max_pixels) {
    ovr_ratio <- sqrt((xsize * ysize) / max_pixels)
    xsize <- trunc(xsize / ovr_ratio)
    ysize <- trunc(ysize / ovr_ratio)
    xsize <- xsize # Update xsize to match actual output
    ysize <- ysize # Update ysize to match actual output
    warning(
      "'xsize * ysize' exceeds 'max_pixels', downsampling applied",
      call. = FALSE
    )
  }

  for (b in bands) {
    if (data$getDataTypeName(b) != "Byte") {
      is_byte_raster <- FALSE
    }
  }

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

  nbands <- length(bands)

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

  if (nbands == 1 && is.null(col_tbl) && is.null(col)) {
    if (
      !is.null(data$getColorTable(band = 1)) &&
        data$getPaletteInterp(band = 1) == "RGB"
    ) {
      col_tbl <- data$getColorTable(band = 1)
      maxColorValue <- 255
    }
  }
  gt <- data$getGeoTransform()
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

  if (
    nbands == 3 &&
      is_byte_raster &&
      normalize &&
      is.null(minmax_def) &&
      is.null(minmax_pct_cut)
  ) {
    minmax_def <- c(0, 0, 0, 255, 255, 255)
  }

  if (typeof(data_in) == "raw") {
    data_in <- as.integer(data_in)
  }

  if (!is.null(pixel_fn)) {
    data_in <- pixel_fn(data_in)
  }

  if (typeof(data_in) == "complex") {
    stop("specify 'pixel_fn' when plotting complex data types", call. = FALSE)
  }

  if (south_up) {
    data_in <- gdalraster:::.flip_vertical(
      data_in,
      xsize,
      ysize,
      nbands
    )
  }

  # Create discrete-aware color mapping for both main plot and legend
  final_col_map_fn <- col_map_fn
  use_normalization <- normalize # Default to original normalize parameter
  is_discrete_data <- FALSE

  if (nbands == 1 && is.null(col_tbl) && !is.null(col_map_fn)) {
    unique_vals <- unique(data_in[!is.na(data_in)])
    n_unique_vals <- length(unique_vals)

    if (n_unique_vals < 12) {
      # This is discrete data
      is_discrete_data <- TRUE
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
      final_col_map_fn <- function(x) {
        # For each input value, find which discrete value it matches
        result <- character(length(x))
        for (i in seq_along(unique_vals_sorted)) {
          mask <- abs(x - unique_vals_sorted[i]) < 1e-10
          result[mask] <- discrete_colors[i]
        }
        # Handle any unmatched values (shouldn't happen, but safety)
        unmatched <- result == ""
        if (any(unmatched)) {
          result[unmatched] <- discrete_colors[1]
        }
        return(result)
      }
    }
  }

  a <- array(data_in, dim = c(xsize, ysize, nbands))
  r <- gdalraster:::.as_raster(
    a,
    col_tbl = col_tbl,
    maxColorValue = maxColorValue,
    normalize = use_normalization, # Use conditional normalization
    minmax_def = minmax_def,
    minmax_pct_cut = minmax_pct_cut,
    col_map_fn = final_col_map_fn,
    na_col = na_col
  )
  if (legend && nbands != 1) {
    message("legend is not supported for RGB plot")
    legend <- FALSE
  }

  if (legend) {
    # Save only the specific parameters we'll modify
    op_mar <- graphics::par("mar")
    on.exit(graphics::par(mar = op_mar))

    # Use balanced margins with user offset - all sides proportional
    base_mar <- c(4, 4, 3, 1) + 0.1
    graphics::par(mar = base_mar + mar) # apply user margin offset
  }

  graphics::plot.new()

  if (legend) {
    # Adjust xlim to accommodate legend - extend by 20% for more legend space
    xlim_extended <- c(xlim[1], xlim[1] + 1.20 * diff(xlim))
  } else {
    xlim_extended <- xlim
  }

  graphics::plot.window(
    xlim = xlim_extended,
    ylim = ylim,
    asp = 1,
    xaxs = "i", # hardcoded for tight axes
    yaxs = "i", # hardcoded for tight axes
    ...
  )
  graphics::rasterImage(
    r,
    xlim[1],
    ylim[1],
    xlim[2], # use original xlim[2] so image keeps proper aspect ratio
    ylim[2],
    interpolate = interpolate
  )
  if (axes) {
    # Always use consistent axis behavior regardless of legend/band count
    plot_center_x <- mean(xlim)

    # Position titles relative to canvas edges but with more spacing
    graphics::mtext(
      main,
      side = 3,
      line = 0.5,
      at = plot_center_x,
      cex = 1.8,
      font = 2
    ) # main title closer to plot
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
      lwd = 0, # no axis line
      lwd.ticks = 1, # just ticks
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
      lwd = 0, # no axis line
      lwd.ticks = 1, # just ticks
      cex.axis = 1.2
    )

    # Add lines to complete the box around plot extent
    graphics::segments(xlim[1], ylim[2], xlim[2], ylim[2]) # top line
    graphics::segments(xlim[2], ylim[1], xlim[2], ylim[2]) # right line
    # Extend bottom and left axis lines to full plot extent
    graphics::segments(xlim[1], ylim[1], xlim[2], ylim[1])
    graphics::segments(xlim[1], ylim[1], xlim[1], ylim[2])
  } else {
    # No axes case - still position title consistently with more spacing from
    # canvas edge
    plot_center_x <- mean(xlim)
    graphics::mtext(
      main,
      side = 3,
      line = 0.5,
      at = plot_center_x,
      cex = 1.8,
      font = 2
    ) # main title closer to plot
  }
  if (legend) {
    mm <- NULL
    if (!is.null(minmax_def)) {
      mm <- minmax_def
    } else if (!is.null(minmax_pct_cut)) {
      mm <- stats::quantile(
        data_in,
        probs = c(minmax_pct_cut[1] / 100, minmax_pct_cut[2] / 100),
        na.rm = TRUE,
        names = FALSE
      )
    } else {
      mm <- c(min(data_in, na.rm = TRUE), max(data_in, na.rm = TRUE))
    }

    # Check for discrete vs continuous legend
    unique_values <- unique(data_in[!is.na(data_in)])
    n_unique <- length(unique_values)
    is_discrete <- n_unique < 12

    if (is.null(col_tbl)) {
      if (is_discrete) {
        # Discrete legend: remap values to sequential indices for distinct colors
        unique_values <- sort(unique_values)

        # For discrete data, use sequential positions to get distinct colors
        # This prevents interpolation between widely spaced values
        if (n_unique > 1) {
          # Map to evenly spaced positions in [0,1] for distinct colors
          normalized_values <- seq(0, 1, length.out = n_unique)
        } else {
          normalized_values <- 0.5 # single value gets middle color
        }

        # Get distinct colors for each unique value (no interpolation)
        colors_for_values <- col_map_fn(normalized_values)

        # Create distinct color segments - each gets equal space in the legend
        legend_height <- 256
        pixels_per_value <- floor(legend_height / n_unique)

        # Build the color vector by creating blocks for each unique value
        leg_colors <- character(legend_height)
        for (i in 1:n_unique) {
          start_idx <- (i - 1) * pixels_per_value + 1
          end_idx <- min(i * pixels_per_value, legend_height)
          leg_colors[start_idx:end_idx] <- colors_for_values[i]
        }

        # Fill any remaining pixels with the last color
        if (any(leg_colors == "")) {
          leg_colors[leg_colors == ""] <- colors_for_values[n_unique]
        }

        # Reverse for top-to-bottom display (highest values at top)
        leg_colors <- rev(leg_colors)
        leg_img <- grDevices::as.raster(matrix(leg_colors, ncol = 1))
      } else {
        # Continuous legend: use interpolated values
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

    # Draw legend in the extended plot area, closer to the image
    legend_x_start <- xlim[2] + 0.02 * diff(xlim) # smaller gap after image
    legend_x_end <- xlim[2] + 0.07 * diff(xlim) # legend bar width

    # Make legend shorter to accommodate labels - use 80% of height, centered
    y_range <- diff(ylim)
    legend_y_start <- ylim[1] + 0.1 * y_range # 10% margin from bottom
    legend_y_end <- ylim[2] - 0.1 * y_range # 10% margin from top

    # Draw the legend raster
    graphics::rasterImage(
      leg_img,
      legend_x_start,
      legend_y_start,
      legend_x_end,
      legend_y_end
    )

    # Add a clean outline around the legend color bar
    graphics::rect(
      legend_x_start,
      legend_y_start,
      legend_x_end,
      legend_y_end,
      border = "black",
      lwd = 1
    )

    # Add legend labels
    if (is_discrete) {
      # Discrete legend: show all unique values
      if (is(unique_values, "integer")) {
        leg_lab <- formatC(rev(unique_values), format = "d")
      } else {
        leg_lab <- formatC(rev(unique_values), format = "f", digits = digits)
      }
      # Position labels at the center of each color block
      legend_height_plot <- legend_y_end - legend_y_start
      block_height <- legend_height_plot / n_unique
      # Calculate center positions for each block (from top to bottom)
      text_y <- seq(
        legend_y_end - block_height / 2, # center of top block
        legend_y_start + block_height / 2, # center of bottom block
        length.out = n_unique
      )
    } else {
      # Continuous legend: show 5 evenly spaced values
      if (is(data_in, "integer")) {
        leg_lab <- formatC(seq(mm[1], mm[2], length.out = 5), format = "d")
      } else {
        leg_lab <- formatC(
          seq(mm[1], mm[2], length.out = 5),
          format = "f",
          digits = digits
        )
      }
      # Position labels evenly across legend height
      text_y <- seq(legend_y_start, legend_y_end, length.out = 5)
    }

    # Position text labels to the right of the color bar, matching legend height
    text_x <- xlim[2] + 0.09 * diff(xlim) # adjusted for closer legend position

    # Add tick marks on the right side of the legend aligned with labels
    tick_length <- 0.01 * diff(xlim) # small tick length
    graphics::segments(
      legend_x_end,
      text_y,
      legend_x_end + tick_length,
      text_y,
      lwd = 1
    )

    graphics::text(
      text_x,
      text_y,
      labels = leg_lab,
      adj = c(0, 0.5),
      cex = 1.2
    )
  }
  invisible()
}
