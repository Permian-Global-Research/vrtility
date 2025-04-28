#' @param nstart Number of times the algorithm is ran over all the data set.
#' @param gamma Value (positive) of the constant controling the descent steps
#' see details in \code{\link[Gmedian]{Gmedian}}.
#' @param alpha Rate of decrease of the descent steps (see details). Should
#' satisfy \eqn{1/2< alpha <= 1}.
#' @param epsilon Numerical tolerance. By defaut set to 1e-08.
#' @param impute_na Logical. If TRUE, missing values are replaced with the
#' an appropriate band-level statistic. If FALSE, missing values are not
#' replaced. which may result in NA values in the output for all bands.
#' @details The `geomedian` function wraps \code{\link[Gmedian]{Gmedian}}  and
#' is identical other than it uses the column medians as the initial
#' starting value for the algorithm, rather than the first row of the matrix.
#' @rdname multiband_reduce
#' @export
geomedian <- function(
  nstart = 5,
  gamma = 10,
  alpha = 0.65,
  epsilon = 1e-8,
  impute_na = TRUE
) {
  function(x) {
    colstat <- matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1)
    if (impute_na) {
      na_indices <- which(is.na(x), arr.ind = TRUE)
      if (length(na_indices) > 0) {
        x[na_indices] <- colstat[na_indices[, 2]]
      }
    }

    as.vector(
      Gmedian::Gmedian(
        x,
        init = colstat,
        nstart = nstart,
        gamma = gamma,
        alpha = alpha,
        epsilon = epsilon
      )
    )
  }
}

#' @param nitermax Maximum number of iterations.
#' @rdname multiband_reduce
#' @details The `geomedian_weizfeld` function wraps
#' \code{\link[Gmedian]{Weiszfeld}} and is idential but does not provide support
#' for weights.
#' @export
geomedian_weizfeld <- function(
  epsilon = 1e-8,
  nitermax = 100,
  impute_na = TRUE
) {
  function(x) {
    if (impute_na) {
      colstat <- matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1)
      na_indices <- which(is.na(x), arr.ind = TRUE)
      if (length(na_indices) > 0) {
        x[na_indices] <- colstat[na_indices[, 2]]
      }
    }
    as.vector(
      Gmedian::Weiszfeld(
        x,
        nitermax = nitermax,
        epsilon = epsilon
      )$median
    )
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
  # TODO: something isnt right check using the hls example
  distance_type <- rlang::arg_match(distance_type)
  # Pre-select the return function based on impute_na
  return_fn <- return_impute(impute_na)

  handle_na <- handle_impute(
    impute_na,
    function(x) {
      Rfast::colMedians(x, na.rm = TRUE)
    },
    function(x) {
      Rfast::colMedians(x, na.rm = FALSE)
    }
  )

  function(x) {
    # create copy
    xc <- handle_na(x)

    nearest <- Rfast::dista(
      xc$xc,
      xc$colstat,
      type = distance_type
    )
    return_fn(x, xc, which.min(nearest))
  }
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
  # Pre-select the return function based on impute_na
  return_fn <- return_impute(impute_na)
  handle_na <- handle_impute(
    impute_na,
    function(x) {
      WGCNA::colQuantileC(x, p = probability)
    },
    function(x) {
      WGCNA::colQuantileC(x, p = probability)
    }
  )

  function(x) {
    # create copy
    xc <- handle_na(x)

    nearest <- Rfast::dista(
      xc$xc,
      xc$colstat,
      type = distance_type
    )
    return_fn(x, xc, which.min(nearest))
  }
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
  nstart = 5,
  gamma = 10,
  alpha = 0.65,
  epsilon = 1e-8,
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
  #TODO:NA handling not correct
  distance_type <- rlang::arg_match(distance_type)
  function(x) {
    colmeds <- Rfast::colMedians(x, na.rm = TRUE)
    gmed <- Gmedian::Gmedian(
      x,
      init = colmeds,
      nstart = nstart,
      gamma = gamma,
      alpha = alpha,
      epsilon = epsilon
    )

    nearest <- Rfast::dista(
      x,
      gmed,
      type = distance_type
    )
    x[which.min(nearest), ]
  }
}

return_impute <- function(impute_na) {
  f <- if (impute_na) {
    function(x, xc, i) xc$xc[i, ]
  } else {
    function(x, xc, i) {
      x[i, ]
    }
  }
  return(f)
}
handle_impute <- function(impute_na, stat_t, stat_f) {
  handle_na <- if (impute_na) {
    function(xc) {
      colstat <- matrix(stat_t(xc), nrow = 1)
      na_indices <- which(is.na(xc), arr.ind = TRUE)
      if (length(na_indices) > 0) {
        xc[na_indices] <- colstat[na_indices[, 2]]
      }
      return(list(xc = xc, colstat = colstat))
    }
  } else {
    function(xc) {
      colstat <- matrix(stat_f(xc), nrow = 1)
      colstat <- colstat[, colSums(is.na(xc)) == 0]
      # drop columsn with na
      xc <- xc[, colSums(is.na(xc)) == 0]
      return(list(xc = xc, colstat = colstat))
    }
  }
  return(handle_na)
}
