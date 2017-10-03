#include <Rcpp.h>

using namespace Rcpp;

//' @title Merge data frame columns
//' @description TBD
//' @param df Dataframe
//' @param cols Vector
//' @param sep Character seperator
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
std::vector<std::string> merge_columns_c(DataFrame df, CharacterVector cols, std::string sep = ".") {
  int vRows = df.nrows();

  std::vector<std::string> newCol( vRows );

  List colList;
  for (int j = 0; j < cols.length(); j++ ) {
    std::ostringstream oss;
    oss << cols[j];
    std::string col = oss.str();
    Rcpp::CharacterVector cv = df[col];
    colList[col] = cv;
  }

  CharacterVector colNames = colList.names();
  for (int i = 0; i < vRows; i++ ) {
    std::ostringstream ossCol;
    for (int j = 0; j < colNames.length(); j++ ) {
      std::ostringstream oss;
      oss << cols[j];
      std::string colName = oss.str();
      CharacterVector colVec = colList[colName];

      ossCol << colVec[i];
      if(j + 1 < colNames.length()) {
        ossCol << sep;
      }
    }

    newCol[i] = ossCol.str();
  }

  return newCol;
}
