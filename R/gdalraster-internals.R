# Internal plotting helpers adapted from gdalraster (MIT license).
# These are re-implemented here to avoid using `:::` on gdalraster internals,
# which is not permitted on CRAN.
# Original source: https://github.com/USDAForestService/gdalraster
#
# .gr_normalize and .gr_as_raster are direct copies of the R functions.
# .gr_flip_vertical is a pure-R reimplementation of the C++ original.

#' Normalize values to 0-1 range
#' @noRd
#' @keywords internal
.gr_normalize <- function(x, minmax = NULL) {
  if (is.null(minmax)) {
    xn <- (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else {
    xn <- (x - minmax[1]) / (minmax[2] - minmax[1])
    xn[xn < 0] <- 0
    xn[xn > 1] <- 1
  }
  return(xn)
}

#' Convert a 3D array to an R raster object for plotting
#' @noRd
#' @keywords internal
.gr_as_raster <- function(
    a,
    col_tbl = NULL,
    maxColorValue = 1,
    normalize = TRUE,
    minmax_def = NULL,
    minmax_pct_cut = NULL,
    col_map_fn = NULL,
    na_col = grDevices::rgb(0, 0, 0, 0),
    ...) {
  nbands <- dim(a)[3]
  if (!(nbands %in% c(1, 3))) {
    stop("number of bands must be 1 or 3", call. = FALSE)
  }

  r <- array()

  if (!is.null(col_tbl)) {
    if (nbands != 1) {
      stop("color table can only be used with single-band data", call. = FALSE)
    }
    ct <- as.data.frame(col_tbl)
    if (ncol(ct) < 4 || ncol(ct) > 5) {
      stop("color table must have four or five columns", call. = FALSE)
    }
    if (ncol(ct) == 4) ct[, 5] <- rep(maxColorValue, nrow(ct))
    ct[, 6] <- grDevices::rgb(
      ct[, 2], ct[, 3], ct[, 4], ct[, 5],
      maxColorValue = maxColorValue
    )
    names(ct) <- c("value", "r", "g", "b", "a", "rgb")
    r <- ct$rgb[match(as.vector(a), ct$value)]
    if (anyNA(r)) r[is.na(r)] <- na_col
    dim(r) <- dim(a)[2:1]
    class(r) <- "raster"
  } else {
    if (normalize) {
      if (!is.null(minmax_def)) {
        for (b in 1:nbands) {
          a[, , b] <- .gr_normalize(a[, , b], minmax_def[c(b, b + nbands)])
        }
      } else if (!is.null(minmax_pct_cut)) {
        for (b in 1:nbands) {
          q <- stats::quantile(
            a[, , b],
            probs = c(minmax_pct_cut[1] / 100, minmax_pct_cut[2] / 100),
            na.rm = TRUE, names = FALSE
          )
          a[, , b] <- .gr_normalize(a[, , b], q)
        }
      } else {
        for (b in 1:nbands) {
          a[, , b] <- .gr_normalize(a[, , b])
        }
      }
    }

    if (is.null(col_map_fn)) {
      col_map_fn <- ifelse(nbands == 1, grDevices::gray, grDevices::rgb)
    }

    has_na <- FALSE
    nas <- array()
    if (anyNA(a)) {
      has_na <- TRUE
      nas <- is.na(a)
      a[nas] <- 0
      if (dim(nas)[3] == 3) {
        nas <- nas[, , 1] | nas[, , 2] | nas[, , 3]
      }
      dim(nas) <- dim(nas)[1:2]
      nas <- t(nas)
    }

    if (nbands == 1) {
      dim(a) <- dim(a)[1:2]
      r <- col_map_fn(a)
      dim(r) <- dim(a)[2:1]
      class(r) <- "raster"
    } else {
      r <- col_map_fn(a[, , 1], a[, , 2], a[, , 3])
      dim(r) <- dim(a)[2:1]
      class(r) <- "raster"
    }

    if (has_na) r[nas] <- na_col
  }

  return(r)
}

#' Flip raster data vertically (reverse row order)
#'
#' Pure-R reimplementation of gdalraster's C++ `.flip_vertical`.
#' Takes a flat vector of raster data (band-sequential) and reverses the
#' row order within each band.
#' @noRd
#' @keywords internal
.gr_flip_vertical <- function(v, xsize, ysize, nbands) {
  band_size <- xsize * ysize
  for (b in seq_len(nbands)) {
    offset <- (b - 1L) * band_size
    idx <- offset + seq_len(band_size)
    # R fills column-major: nrow=xsize gives each column = one image row
    mat <- matrix(v[idx], nrow = xsize, ncol = ysize)
    v[idx] <- as.vector(mat[, ysize:1, drop = FALSE])
  }
  v
}
