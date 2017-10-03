#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//' Calculates the upper triangle of a vector of integers  if it
//' were converted to a matrix. This actually skips creating the
//' matrix, by only multiplying the necesseary indices of the
//' vector.
//'
//' @param v - A vector of integers


std::vector<int> vector_to_ut(std::vector<int> v) {
  int vL = v.size();
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  int s = 0;
  std::vector<int> vR( vS );
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      vR[s] = v[j] * v[i-1];
      s++;
    }
  }
  return vR;
}


arma::rowvec vector_to_ut_mul(arma::vec v) {
  arma::mat mat = v * trans(v);
  mat.diag().zeros();
  return(trans(mat.elem(find(trans(trimatl(mat))))));
}
