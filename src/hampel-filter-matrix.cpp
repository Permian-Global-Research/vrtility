#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Helper function for efficient median calculation
inline double fast_median(std::vector<double>& v) {
  size_t n = v.size();
  if (n == 0) return NA_REAL;
  
  // Use nth_element for O(n) median finding instead of full sort
  size_t mid = n / 2;
  std::nth_element(v.begin(), v.begin() + mid, v.end());
  
  if (n % 2 == 1) {
    return v[mid];
  } else {
    double mid_val = v[mid];
    std::nth_element(v.begin(), v.begin() + mid - 1, v.end());
    return (v[mid - 1] + mid_val) / 2.0;
  }
}

// [[Rcpp::export]]
NumericMatrix hampel_filter_matrix_cpp(NumericMatrix X, int k, double t0 = 3.0, bool impute_na = false)
{
  int n = X.nrow();
  int p = X.ncol();
  NumericMatrix Y = clone(X);

  // Process each column independently
  for (int col = 0; col < p; col++)
  {
    // Create index mapping for non-NA values
    std::vector<int> valid_indices;
    std::vector<double> valid_values;
    valid_indices.reserve(n);
    valid_values.reserve(n);
    
    for (int i = 0; i < n; i++)
    {
      if (!ISNA(X(i, col)))
      {
        valid_indices.push_back(i);
        valid_values.push_back(X(i, col));
      }
    }

    int valid_n = valid_values.size();
    std::vector<double> filtered_values = valid_values;
    
    if (valid_n >= 2 * k + 1)
    {
      // Process valid values with Hampel filter
      double L = 1.4826;
      int window_size = 2 * k + 1;
      
      // Pre-allocate vectors for window calculations
      std::vector<double> window(window_size);
      std::vector<double> abs_dev(window_size);

      for (int i = k; i < valid_n - k; i++)
      {
        // Copy window values
        for (int j = 0; j < window_size; j++)
        {
          window[j] = valid_values[i - k + j];
        }

        // Calculate median using efficient algorithm
        std::vector<double> window_copy = window;
        double x0 = fast_median(window_copy);
        
        // Calculate absolute deviations
        for (int j = 0; j < window_size; j++)
        {
          abs_dev[j] = std::abs(window[j] - x0);
        }
        
        // Calculate MAD using efficient algorithm
        double S0 = L * fast_median(abs_dev);

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
