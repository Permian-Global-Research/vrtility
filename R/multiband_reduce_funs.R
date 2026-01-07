#' @param nstart Number of times the algorithm is ran over all the data set.
#' Only used if `weizfeld = FALSE`.
#' @param gamma Value (positive) of the constant controlling the descent steps.
#' See details in \code{\link[Gmedian]{Gmedian}}. Only used if
#' `weizfeld = FALSE`.
#' @param alpha Rate of decrease of the descent steps (see details). Should
#' satisfy \eqn{1/2 < alpha <= 1}. Only used if `weizfeld = FALSE`.
#' @param epsilon Numerical tolerance. By default set to 1e-08.
#' @param weizfeld Logical. If TRUE, the Weiszfeld algorithm is used to
#' calculate the geometric median - see \code{\link[Gmedian]{Weiszfeld}}. If
#' FALSE (the default), the Gmedian algorithm is used,
#' see \code{\link[Gmedian]{Gmedian}}. The Gmedian algorithm is faster and
#' intrinsically handles missing values.
#' @param nitermax Maximum number of iterations. By default set to 100. Only
#' used if `weizfeld = TRUE`.
#' @param impute_na Logical. If TRUE, missing values are replaced with an
#' appropriate band-level statistic - in the case of geomedian this is only
#' relevant when `weizfeld = TRUE` - in such a case the Gmedian algorithm is
#' used to estimate bands with NA values. For medoid the column/band median
#' is used; for `quantoid` this will be the requested quantile
#' probability of the column. If FALSE, missing values are not
#' replaced, which may result in NA values in the output for multiple bands.
#' @details
#' ## geomedian
#'
#' Calculates the geometric (spatial) median across all bands. The geometric
#' median is the point minimizing the sum of Euclidean distances to all
#' observations - a multivariate generalization of the median that ensures
#' spectral consistency across bands. Unlike band-by-band medians, the result
#' is a synthetic pixel that may not exist in the original data but is robust
#' to outliers (e.g., clouds, shadows).
#'
#' Two algorithms are available via the `weizfeld` parameter:
#' - `weizfeld = FALSE` (default): Uses \code{\link[Gmedian]{Gmedian}}, a
#'   stochastic gradient descent algorithm that handles NA values intrinsically.
#' - `weizfeld = TRUE`: Uses \code{\link[Gmedian]{Weiszfeld}}, an iterative
#'   algorithm that requires complete cases. When `impute_na = TRUE`, NA bands
#'   are filled using Gmedian estimates.
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


#' @param distance_type The type of distance metric to use. Default is
#' "euclidean". See \code{\link[Rfast]{dista}} for all available metrics.
#'
#' @details
#' ## medoid
#'
#' Selects the observation (row) closest to the band-level medians. Unlike
#' geomedian, this returns actual observed pixel values rather than synthetic
#' statistics, preserving spectral authenticity - useful when real sensor
#' measurements are required.
#'
#' The algorithm:
#' 1. Computes the median for each band (column)
#' 2. Calculates distances from each observation to this median vector
#' 3. Returns the observation with minimum distance
#'
#' When `impute_na = TRUE`, any NA values in the selected observation are
#' replaced with the band median.
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

#' @param probability The quantile probability to use (0-1). Default is 0.4.
#'
#' @details
#' ## quantoid
#'
#' Like medoid, but uses a specified quantile instead of the median for
#' distance calculations. Useful when median filtering is insufficient - for
#' example, a lower quantile (e.g., 0.2-0.4) can better reject bright outliers
#' like clouds by biasing toward darker observations.
#'
#' Requires the WGCNA package. When `impute_na = TRUE`, NA values in the
#' selected observation are replaced with the band quantile.
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

#' @details
#' ## geomedoid
#'
#' Combines geomedian and medoid approaches: calculates the geometric median
#' first, then selects the nearest observed pixel to that synthetic point.
#' This provides:
#' - **Outlier robustness** from the geometric median calculation
#' - **Real pixel values** from the medoid selection
#'
#' More robust to outliers (clouds, shadows) than medoid alone because the
#' target point is a geometric median rather than band-by-band medians. When
#' `impute_na = TRUE`, NA values are filled using geomedian estimates.
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
    if (anyNA(result) && impute_na) {
      result_na_cols <- which(is.na(result))
      na_stat_impute <- impute_f(
        x[, result_na_cols, drop = FALSE]
      )
      result[result_na_cols] <- na_stat_impute
    }

    return(result)
  }
}
