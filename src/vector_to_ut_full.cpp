#include <Rcpp.h>
using namespace Rcpp;

//' Calculates the upper triangle (including the diaganol) of a
//' vector of integers  if it were converted to a matrix. This
//' actually skips creating the matrix, by only multiplying the
//' necesseary indices of the vector.
//'
//' @param v - A vector of integers


std::vector<int> vector_to_ut_full(std::vector<int> v) {
  int vL = v.size();
  int vS = (vL * (vL + 1)) / 2;
  std::vector<int> vR( vS );
  int s = 0;

  for( int i = 1; i <= vL; i++ ) {
    for (int j = 0; j < i; j++ ) {
      vR[s] = v[j] * v[i-1];
      s++;
    }
  }
  return vR;
}
