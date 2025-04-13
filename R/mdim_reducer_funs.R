geomedian <- function(nstart = 5, gamma = 10, alpha = 0.65) {
  # creating the Gamma constant.
  # gamma <- 18 +
  #   (mean(Rfast::colVars(bandmatrix, std = TRUE, na.rm = TRUE)) /
  #     mean(colmeds)) *
  #     10
  function(x) {
    colmeds <- Rfast::colMedians(x, na.rm = TRUE) # do we need this???
    as.vector(
      Gmedian::Gmedian(
        x,
        init = colmeds,
        nstart = nstart,
        gamma = gamma,
        alpha = alpha
      )
    )
  }
}

geomedian_weizfeld <- function(nitermax = 1000) {
  function(x) {
    as.vector(
      Gmedian::Weiszfeld(
        x,
        nitermax = nitermax
      )$median
    )
  }
}

medoid <- function(dist_type = "euclidean") {
  function(x) {
    nearest <- Rfast::dista(
      x,
      matrix(Rfast::colMedians(x, na.rm = TRUE), nrow = 1),
      type = dist_type
    )
    x[which.min(nearest), ]
  }
}

geomedoid <- function(
  nstart = 50,
  gamma = 15,
  alpha = 0.9,
  dist_type = "euclidean"
) {
  function(x) {
    colmeds <- Rfast::colMedians(x, na.rm = TRUE)
    gmed <- Gmedian::Gmedian(
      x,
      init = colmeds,
      nstart = nstart,
      gamma = gamma,
      alpha = alpha
    )

    nearest <- Rfast::dista(
      x,
      gmed,
      type = dist_type
    )
    x[which.min(nearest), ]
  }
}
