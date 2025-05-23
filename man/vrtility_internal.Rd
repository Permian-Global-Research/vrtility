% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiband_reduce.R, R/singleband-many-to-many.R
\name{extract_band_matrices}
\alias{extract_band_matrices}
\alias{matrix_to_rowlist}
\title{Extract band matrices using C++}
\usage{
extract_band_matrices(
  x,
  row_indices,
  col_indices,
  n_cells,
  n_timepoints,
  n_bands
)

matrix_to_rowlist(x)
}
\arguments{
\item{x}{A matrix.}

\item{row_indices}{A vector of row indices.}

\item{col_indices}{A vector of column indices.}

\item{n_cells}{The number of cells.}

\item{n_timepoints}{The number of time points.}

\item{n_bands}{The number of bands.}
}
\value{
A list of matrices, where each matrix corresponds to a band.

A list of vectors, where each vector represents a row of the
matrix.
}
\description{
Internal C++ function wrapped for R

Convert a matrix to a list of vectors, where each vector
represents a row of the matrix.
}
\keyword{internal}
