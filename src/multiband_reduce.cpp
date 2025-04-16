#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List extract_band_matrices_cpp(List x,
                               IntegerVector row_indices,
                               IntegerVector col_indices,
                               int n_cells,
                               int n_timepoints,
                               int n_bands)
{
  List result(n_cells);

  // Cache the time points and bands
  std::vector<List> time_points(n_timepoints);
  std::vector<std::vector<NumericMatrix>> bands(n_timepoints);

  // Pre-load all data to avoid R object access issues
  for (int t = 0; t < n_timepoints; t++)
  {
    time_points[t] = as<List>(x[t]);
    bands[t].resize(n_bands);
    for (int b = 0; b < n_bands; b++)
    {
      bands[t][b] = as<NumericMatrix>(time_points[t][b]);
    }
  }

  // Process cells sequentially - safer but still fast
  for (int k = 0; k < n_cells; k++)
  {
    int i = row_indices[k] - 1;
    int j = col_indices[k] - 1;

    NumericMatrix bandmatrix(n_timepoints, n_bands);
    LogicalVector valid_rows(n_timepoints, true);
    int n_valid_rows = 0;

    // Fill and validate
    for (int t = 0; t < n_timepoints; t++)
    {
      bool row_has_data = false;
      for (int b = 0; b < n_bands; b++)
      {
        double val = bands[t][b](i, j);
        bandmatrix(t, b) = val;
        if (!ISNAN(val))
          row_has_data = true;
      }
      valid_rows[t] = row_has_data;
      if (row_has_data)
        n_valid_rows++;
    }

    // Create result matrix
    if (n_valid_rows > 0)
    {
      NumericMatrix filtered_matrix(n_valid_rows, n_bands);
      int row_idx = 0;
      for (int t = 0; t < n_timepoints; t++)
      {
        if (valid_rows[t])
        {
          for (int b = 0; b < n_bands; b++)
          {
            filtered_matrix(row_idx, b) = bandmatrix(t, b);
          }
          row_idx++;
        }
      }
      result[k] = filtered_matrix;
    }
    else
    {
      NumericMatrix na_matrix(1, n_bands);
      std::fill(na_matrix.begin(), na_matrix.end(), NA_REAL);
      result[k] = na_matrix;
    }
  }

  return result;
}