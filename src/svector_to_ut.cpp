#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::string> svector_to_ut(std::vector<std::string> v) {
  int vL = v.size();
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  int s = 0;

  std::vector<std::string> vR( vS );
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      vR[s] = v[j] + " & " + v[i-1];
      s++;
    }
  }
  return vR;
}
