// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

// @name ref_window_sum
// @title ref_window_sum
// @param df dataframe
// @param binary logical
// @description TBD
// [[Rcpp::export]]
DataFrame ref_window_sum(
  DataFrame df,
  bool binary = true
) {
  int dfCols = df.size();
  int dfRows = df.nrows();

  arma::mat df_CoOccurred(dfRows, dfCols, fill::zeros);
  arma::mat df_AsMatrix2(dfRows, dfCols, fill::zeros);

  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }
  return(sum(df_AsMatrix2));
}
