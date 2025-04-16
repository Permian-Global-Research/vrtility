#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List mdim_reduction_cpp(List x,
                        Function mdim_fun,
                        IntegerVector row_indices,
                        IntegerVector col_indices,
                        int n_cells,
                        int n_timepoints,
                        int n_bands)
{

  List result(n_cells);

  for (int k = 0; k < n_cells; k++)
  {
    int i = row_indices[k] - 1; // Convert to 0-based indexing
    int j = col_indices[k] - 1;

    // Create matrix to store band values
    NumericMatrix bandmatrix(n_timepoints, n_bands);

    // Fill bandmatrix
    for (int t = 0; t < n_timepoints; t++)
    {
      List time_point = as<List>(x[t]);
      for (int b = 0; b < n_bands; b++)
      {
        NumericMatrix band = as<NumericMatrix>(time_point[b]);
        bandmatrix(t, b) = band(i, j);
      }
    }

    // Remove rows with all NA
    LogicalVector keep(n_timepoints, true);
    for (int t = 0; t < n_timepoints; t++)
    {
      bool all_na = true;
      for (int b = 0; b < n_bands; b++)
      {
        if (!ISNA(bandmatrix(t, b)))
        {
          all_na = false;
          break;
        }
      }
      keep[t] = !all_na;
    }

    // Count non-NA rows
    int n_valid = sum(keep);

    if (n_valid > 1)
    {
      // Create filtered matrix
      NumericMatrix filtered(n_valid, n_bands);
      int idx = 0;
      for (int t = 0; t < n_timepoints; t++)
      {
        if (keep[t])
        {
          for (int b = 0; b < n_bands; b++)
          {
            filtered(idx, b) = bandmatrix(t, b);
          }
          idx++;
        }
      }

      // Call R function
      result[k] = mdim_fun(filtered);
    }
    else if (n_valid == 1)
    {
      // Return the single row
      NumericVector single_row(n_bands);
      int idx = 0;
      while (!keep[idx])
        idx++;
      for (int b = 0; b < n_bands; b++)
      {
        single_row[b] = bandmatrix(idx, b);
      }
      result[k] = single_row;
    }
    else
    {
      // Return NA vector
      NumericVector na_vec(n_bands, NA_REAL);
      result[k] = na_vec;
    }
  }

  return result;
}