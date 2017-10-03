#include <Rcpp.h>
using namespace Rcpp;

// Calculates the upper triangle of a vector of integers  if it
// were converted to a matrix. This actually skips creating the
// matrix, by only multiplying the necesseary indices of the
// vector.
//
// @param v - A vector of integers

DataFrame dfvector_to_ut(DataFrame v, CharacterVector nms) {
  int vRows = v.nrows();

  List listOfSums(nms.length());
  for(int j = 0; j < nms.length(); j++) {
    listOfSums[j] = IntegerVector::create(vRows);
  }
  listOfSums.attr("names") = nms;

  IntegerMatrix Am(vRows, nms.length());
  for(int row = 0; row < vRows; row++) {
    int s = 0;

    for(int i = 2; i <= v.length(); i++) {
      for (int j = 0; j < i-1; j++ ) {
        IntegerVector vDF0 = v.at(j);
        IntegerVector vDF1 = v.at(i-1);
        Am(s, row) = vDF0.at(row) * vDF1.at(row);
        s++;
      }
    }
  }
  DataFrame df(Am);

  return df;
}
