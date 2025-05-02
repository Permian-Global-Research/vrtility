#' @param nstart Number of times the algorithm is ran over all the data set.
#' only used if `weizfeld = FALSE`.
#' @param gamma Value (positive) of the constant controling the descent steps
#' see details in \code{\link[Gmedian]{Gmedian}}. Only used if
#' `weizfeld = FALSE`.
#' @param alpha Rate of decrease of the descent steps (see details). Should
#' satisfy \eqn{1/2< alpha <= 1}. Only used if `weizfeld = FALSE`.
#' @param epsilon Numerical tolerance. By defaut set to 1e-08.
#' @param weizfeld Logical. If TRUE, the Weiszfeld algorithm is used to
#' calculate the geometric median - see \code{\link[Gmedian]{Weiszfeld}}. If
#' FALSE (the default), the Gmedian algorithm is used,
#' see \code{\link[Gmedian]{Gmedian}}. The Gmedian algorithm is faster and
#' intrinsically handles missing values.
#' @param nitermax Maximum number of iterations. By default set to 100. only
#' used if `weizfeld = TRUE`.
#' @param impute_na Logical. If TRUE, missing values are replaced with the
#' an appropriate band-level statistic - in the case of geomedian this is only
#' relevant when `weizfeld = TRUE` - in such a case the Gmedian algorithm is
#' used to estimate bands with NA values.  For medoid the column/band median
#' is used; for `quantoid` this will be the requested quantile
#' probabilioty of the column. If FALSE, missing values are not
#' replaced. which may result in NA values in the output for multiple bands.
#' @details The `geomedian` enables the use of \code{\link[Gmedian]{Gmedian}}
#' and \code{\link[Gmedian]{Weiszfeld}} to calculate the geometric median of a
#' multiband raster. When `Weiszfeld` is set to FALSE,
#' \code{\link[Gmedian]{Gmedian}} is used. For the Gmedian algorithm,
#' the matrix column medians are used as initial values rather than the first
#' row of the matrix.
#' @rdname multiband_reduce
#' @export
geomedian <- function(
  weizfeld = FALSE,
  nitermax = 100,
  nstart = 10,
  gamma = 10,
  alpha = 0.65,
  epsilon = 1e-8,
  impute_na = TRUE
) {
  function(x) {
    if (isFALSE(weizfeld)) {
      # use gmedian (handles NA automatically)
      result <- Gmedian::Gmedian(
        x,
        init = matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1),
        nstart = nstart,
        gamma = gamma,
        alpha = alpha,
        epsilon = epsilon
      )
    } else {
      # use weizfeld
      result <- Gmedian::Weiszfeld(
        x,
        nitermax = nitermax,
        epsilon = epsilon
      )$median

      if (impute_na) {
        na_cols <- which(Rfast::colsums(is.na(x)) > 0)
        na_col_meds <- Gmedian::Gmedian(
          x,
          init = matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1),
          nstart = nstart,
          gamma = gamma,
          alpha = alpha,
          epsilon = epsilon
        )[, na_cols, drop = FALSE]

        result[na_cols] <- na_col_meds
      }
    }

    return(result)
  }
}


#' @param distance_type The type of distance metric to use. See
#' \code{\link[Rfast]{dista}} for a full description of options.
#' @details The `medoid` function  uses \code{\link[Rfast]{dista}} to compute
#' the distance between the band-level medians and the values for each pixel. It
#' then selects the pixel with the minimum distance as the medoid. The returned
#' band pixel values are spectrally consistent and observed rather than
#' synthetic.
#' @rdname multiband_reduce
#' @export
medoid <- function(
  distance_type = c(
    "euclidean",
    "manhattan",
    "minimum",
    "maximum",
    "minkowski",
    "bhattacharyya",
    "hellinger",
    "kullback_leibler",
    "jensen_shannon",
    "canberra",
    "chi_square",
    "soergel",
    "sorensen",
    "cosine",
    "wave_hedges",
    "motyka",
    "harmonic_mean",
    "jeffries_matusita",
    "gower",
    "kulczynski",
    "itakura_saito"
  ),
  impute_na = TRUE
) {
  distance_type <- rlang::arg_match(distance_type)
  xoid_generator(
    f = function(x) {
      Rfast::colMedians(x, na.rm = TRUE)
    },
    distance_type = distance_type,
    impute_na = impute_na
  )
}

#' @param probability The probability of the quantile to use. Default is 0.4.
#' @details The quantoid is equivalent to the medoid but uses a specified
#' quantile value for calculating the distances.
#' @export
#' @rdname multiband_reduce
quantoid <- function(
  distance_type = c(
    "euclidean",
    "manhattan",
    "minimum",
    "maximum",
    "minkowski",
    "bhattacharyya",
    "hellinger",
    "kullback_leibler",
    "jensen_shannon",
    "canberra",
    "chi_square",
    "soergel",
    "sorensen",
    "cosine",
    "wave_hedges",
    "motyka",
    "harmonic_mean",
    "jeffries_matusita",
    "gower",
    "kulczynski",
    "itakura_saito"
  ),
  probability = 0.4,
  impute_na = TRUE
) {
  if (!rlang::is_installed("WGCNA")) {
    cli::cli_abort(
      c(
        "The `quantoid` function requires the 'WGCNA' package.",
        "i" = "Install it with:",
        " " = "{cli::code_highlight('install.packages(\"WGCNA\")')}"
      )
    )
  }

  distance_type <- rlang::arg_match(distance_type)

  xoid_generator(
    f = function(x) {
      WGCNA::colQuantileC(x, p = probability)
    },
    distance_type = distance_type,
    impute_na = impute_na
  )
}

#' @details The `geomedoid` function combines the `geomedian` and `medoid` - it
#' first calculates the geometric median across all bands and then uses this
#' to determine the nearest pixel value to the geometric median. As the
#' geometric median has greater resilience to outliers than the band-level
#' median, this function may selct a medoid value that is less likely to contian
#' clouds or other outliers. The returned band pixel values are spectrally
#' consistent.
#' @export
#' @rdname multiband_reduce
geomedoid <- function(
  distance_type = c(
    "euclidean",
    "manhattan",
    "minimum",
    "maximum",
    "minkowski",
    "bhattacharyya",
    "hellinger",
    "kullback_leibler",
    "jensen_shannon",
    "canberra",
    "chi_square",
    "soergel",
    "sorensen",
    "cosine",
    "wave_hedges",
    "motyka",
    "harmonic_mean",
    "jeffries_matusita",
    "gower",
    "kulczynski",
    "itakura_saito"
  ),
  nstart = 5,
  gamma = 10,
  alpha = 0.65,
  epsilon = 1e-8,
  impute_na = TRUE
) {
  distance_type <- rlang::arg_match(distance_type)

  xoid_generator(
    f = function(x) {
      Gmedian::Gmedian(
        x,
        init = matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1),
        nstart = nstart,
        gamma = gamma,
        alpha = alpha,
        epsilon = epsilon
      )
    },
    distance_type = distance_type,
    impute_na = impute_na
  )
}

#' Function factory to create xoid family functions
#' these functions are used for multi-band reduction.
#' @keywords internal
#' @noRd
xoid_generator <- function(f, distance_type, impute_na, impute_f = f) {
  function(x) {
    # Track NA columns and create working copy
    na_cols <- which(Rfast::colsums(is.na(x)) > 0)
    non_na_cols <- setdiff(seq_len(ncol(x)), na_cols)
    xc <- x[, non_na_cols, drop = FALSE]

    # Get medians for non-NA columns
    colstat <- matrix(f(xc), nrow = 1)

    # Calculate distances using only non-NA columns
    nearest <- Rfast::dista(
      xc,
      colstat,
      type = distance_type
    )

    # Get the best row index
    best_row <- which.min(nearest)

    # Construct result
    result <- x[best_row, ]

    # Replace NA columns with their medians
    if (any(is.na(result)) && impute_na) {
      result_na_cols <- which(is.na(result))
      na_stat_impute <- impute_f(
        x[, result_na_cols, drop = FALSE]
      )
      result[result_na_cols] <- na_stat_impute
    }

    return(result)
  }
}
