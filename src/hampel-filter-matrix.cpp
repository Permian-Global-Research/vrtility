#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix hampel_filter_matrix_cpp(NumericMatrix X, int k, double t0 = 3.0, bool impute_na = false)
{
  int n = X.nrow();
  int p = X.ncol();
  NumericMatrix Y = clone(X);

  // Process each column independently
  for (int col = 0; col < p; col++)
  {
    // Extract column as vector
    NumericVector x(n);
    for (int i = 0; i < n; i++)
    {
      x[i] = X(i, col);
    }

    // Create index mapping for non-NA values
    std::vector<int> valid_indices;
    NumericVector valid_values;
    for (int i = 0; i < n; i++)
    {
      if (!ISNA(x[i]))
      {
        valid_indices.push_back(i);
        valid_values.push_back(x[i]);
      }
    }

    int valid_n = valid_values.size();
    NumericVector filtered_values = clone(valid_values);
    if (valid_n >= 2 * k + 1)
    {

      // Process valid values with Hampel filter

      double L = 1.4826;

      for (int i = k; i < valid_n - k; i++)
      {
        NumericVector window(2 * k + 1);
        for (int j = 0; j < 2 * k + 1; j++)
        {
          window[j] = valid_values[i - k + j];
        }

        double x0 = median(window);
        NumericVector abs_dev(window.length());
        for (int j = 0; j < window.length(); j++)
        {
          abs_dev[j] = std::abs(window[j] - x0);
        }
        double S0 = L * median(abs_dev);

        if (std::abs(valid_values[i] - x0) > t0 * S0)
        {
          filtered_values[i] = x0;
        }
      }
    }

    // Copy filtered values back to result matrix
    for (int i = 0; i < valid_n; i++)
    {
      Y(valid_indices[i], col) = filtered_values[i];
    }

    // Impute NA values using previous non-NA value (LOCF)
    if (impute_na && n > 1)
    {
      for (int i = 0; i < n; i++)
      {
        if (ISNA(Y(i, col))) // Use proper indexing with row and column
        {
          // Find nearest non-NA value before current position
          int before = -1;
          for (int j = i - 1; j >= 0; j--)
          {
            if (!ISNA(Y(j, col))) // Use proper indexing with row and column
            {
              before = j;
              break;
            }
          }

          // If found a previous non-NA value, use it
          if (before >= 0)
          {
            // Use previous value with proper indexing
            Y(i, col) = Y(before, col);
          }
        }
      }
    }
  }
  return Y;
}
