#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List matrix_to_rowlist_cpp(Rcpp::NumericMatrix mat)
{
  int nrows = mat.nrow();
  int ncols = mat.ncol();
  Rcpp::List result(nrows);

  for (int i = 0; i < nrows; i++)
  {
    Rcpp::NumericVector row(ncols);

    // Column-major access: iterate through columns
    for (int j = 0; j < ncols; j++)
    {
      row[j] = mat(i, j);
    }

    result[i] = row;
  }

  return result;
}
