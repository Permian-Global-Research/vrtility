vals_to_array <- function(r) {
  dims <- attributes(r)$gis$dim
  rows <- dims[1]
  cols <- dims[2]
  array(r, dim = c(rows, cols, 3))
}


gamma_trans <- function(r) {
  attribs <- attributes(r)
  band_arrays <- vals_to_array(r)
  # Apply gamma correction
  gamma_values <- c(1.3, 1.1, 1.0) #  brighten red/green for natural look

  # nolint start
  for (band in 1:3) {
    band_data <- band_arrays[,, band]
    valid_data <- band_data[!is.na(band_data)]

    if (length(valid_data) > 0) {
      # Apply gamma correction directly to normalized data (0-1 range)
      band_arrays[,, band] <- band_data^(1 / gamma_values[band])
    }
  }
  # nolint end

  r <- as.vector(band_arrays)
  attributes(r) <- attribs

  return(r)
}


# Histogram equalization function
histeq_trans <- function(r) {
  attribs <- attributes(r)
  band_arrays <- vals_to_array(r)

  # nolint start
  for (band in 1:3) {
    band_data <- band_arrays[,, band]
    valid_data <- band_data[!is.na(band_data)]

    if (length(valid_data) > 0) {
      # Create ECDF for this band using all valid data
      band_ecdf <- ecdf(valid_data)

      # Apply histogram equalization
      band_arrays[,, band] <- band_ecdf(band_data)
    }
  }
  # nolint end

  r <- as.vector(band_arrays)
  attributes(r) <- attribs
  return(r)
}


hist_all_trans <- function(r) {
  attribs <- attributes(r)
  # Create ECDF for this band using all valid data
  rgb_ecdf <- ecdf(r)

  r <- rgb_ecdf(r)
  attributes(r) <- attribs
  return(r)
}
