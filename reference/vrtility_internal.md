# Extract band matrices using C++

Internal C++ function wrapped for R

Convert a matrix to a list of vectors, where each vector represents a
row of the matrix.

## Usage

``` r
extract_band_matrices(
  x,
  row_indices,
  col_indices,
  n_cells,
  n_timepoints,
  n_bands
)

matrix_to_rowlist(x)
```

## Arguments

- x:

  A matrix.

- row_indices:

  A vector of row indices.

- col_indices:

  A vector of column indices.

- n_cells:

  The number of cells.

- n_timepoints:

  The number of time points.

- n_bands:

  The number of bands.

## Value

A list of matrices, where each matrix corresponds to a band.

A list of vectors, where each vector represents a row of the matrix.
