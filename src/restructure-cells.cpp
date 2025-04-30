#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector restructure_cells_cpp(List cell_vals)
{
  // Get dimensions
  int n_bands = Rf_length(cell_vals[0]);
  int n_cells = cell_vals.length();

  // Pre-allocate result vector
  NumericVector result(n_bands * n_cells);

  // Calculate start indices for each band
  std::vector<int> band_starts(n_bands);
  for (int b = 0; b < n_bands; b++)
  {
    band_starts[b] = b * n_cells;
  }

  // Fill result vector by band
  for (int band = 0; band < n_bands; band++)
  {
    int start_idx = band_starts[band];

    // Process each cell
    for (int cell = 0; cell < n_cells; cell++)
    {
      SEXP cell_val = cell_vals[cell];
      double value;

      // Handle potential NA/errors
      if (Rf_isNull(cell_val))
      {
        value = NA_REAL;
      }
      else
      {
        NumericVector vec(cell_val);
        value = (band < vec.length()) ? vec[band] : NA_REAL;
      }

      result[start_idx + cell] = value;
    }
  }

  return result;
}
