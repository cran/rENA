// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;


Rcpp::NumericMatrix eq_pos(Rcpp::CharacterVector names, Rcpp::CharacterMatrix labels, arma::mat rotated, bool plusOne) {
  int numNames = names.size();

  arma::mat vals = arma::mat( size(rotated), fill::eye);
  arma::mat out = arma::mat(numNames, rotated.n_cols, fill::zeros);

  if(plusOne == true) {
    for (int p=0; p < numNames; p++) {
      Rcpp::CharacterVector topRow = labels(0, _);
      Rcpp::CharacterVector botRow = labels(1, _);
      for(int j=0; j < topRow.size(); j++) {
        if(topRow[j] == names[p] || botRow[j] == names[p]) {
          out(p, j) = 1;
        }
      }
    }

    out = (( (out * 2) * rotated ) / (numNames - 1) / 2);
  }

  return Rcpp::wrap(out);
}
