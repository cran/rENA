// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>
#include <vector>
#include <ctime>
#include <algorithm>
#include <iterator>
#include <cmath>
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;
using namespace std;

//' Merge data frame columns
//' @title Merge data frame columns
//' @description TBD
//' @param df Dataframe
//' @param cols Vector
//' @param sep Character seperator
//' @export
// [[Rcpp::export]]
std::vector<std::string> merge_columns_c(
    DataFrame df,
    CharacterVector cols,
    std::string sep = "."
) {
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

NumericMatrix toNumericMatrix(DataFrame x) {
  int nRows=x.nrows();
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }
  return y;
}

//' Upper Triangle from Vector
//'
//' @title vector to upper triangle
//' @description TBD
//' @param v [TBD]
//' @export
// [[Rcpp::export]]
arma::rowvec vector_to_ut(arma::mat v) {
  int vL = v.size();
  int vS = ( (vL * (vL + 1)) / 2) - vL;

  arma::rowvec vR2( vS, fill::zeros );
  int s = 0;
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      vR2[s] = v[j] * v[i-1];
      s++;
    }
  }
  return vR2;
}

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

// [[Rcpp::export]]
arma::mat rows_to_co_occurrences(DataFrame df, bool binary = true) {
  int dfRows = df.nrows();
  int dfCols = df.size();
  int numCoOccurences = ( (dfCols * (dfCols + 1)) / 2) - dfCols;

  arma::mat df_AsMatrix2(dfRows, dfCols, fill::zeros);
  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  arma::mat df_CoOccurred(dfRows, numCoOccurences, fill::zeros);
  for(int row = 0; row < dfRows; row++) {
    df_CoOccurred.row(row) = vector_to_ut(df_AsMatrix2.row(row));
  }

  if(binary == true) {
    df_CoOccurred.elem( find(df_CoOccurred > 0) ).ones();
  }

  return df_CoOccurred;
}

// @title ref_window_df
// @name ref_window_df
// @description TBD
// @param df A dataframe
// @param windowSize Integer for number of rows in the stanza window
// @param windowForward Integer for number of rows in the stanza window forward
// @param binary Logical, treat codes as binary or leave as weighted
// @param binaryStanzas Logical, treat codes as binary or leave as weighted
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
DataFrame ref_window_df(
    DataFrame df,
    float windowSize = 1,
    float windowForward = 0,
    bool binary = true,
    bool binaryStanzas = false
  ) {
  int dfRows = df.nrows();
  int dfCols = df.size();
  int numCoOccurences = ( (dfCols * (dfCols + 1)) / 2) - dfCols;

  arma::mat df_CoOccurred(dfRows, numCoOccurences, fill::zeros);
  arma::mat df_AsMatrix2(dfRows, dfCols, fill::zeros);
  // NumericMatrix df_asNumericMatrix(dfRows, dfCols);

  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  for(int row = 0; row < dfRows; row++) {
    /**
     * The rows in the current window. CurrentRow + Referrants == windowSize
     */

    // NOTE: change the span to always use 0 if infinite window
    int earliestRow = 0, lastRow = row;

    if (windowSize == std::numeric_limits<double>::infinity()) {
      earliestRow = 0;
    }
    else if (windowSize == 0) {
      earliestRow = row;
    }
    else if ( row - (windowSize-1) >= 0 ) {
      earliestRow = row - (windowSize - 1);
    }

    if (windowForward == std::numeric_limits<double>::infinity()) {
      lastRow = dfRows-1;
    } else if ( windowForward > 0 &&  (row + (windowForward) <= dfRows-1)) {
      lastRow = row + windowForward;
    }

    arma::mat currRows2 = df_AsMatrix2( span( earliestRow, lastRow ), span::all );
    arma::mat currRowsSummed = arma::sum(currRows2);
    arma::rowvec toUT = vector_to_ut(currRowsSummed);
    if(windowSize > 1 && row-1>=0) {
      int headRows = currRows2.n_rows - 1 - windowForward;
      if(headRows < 0) {
        headRows = 0;
      }
      arma::mat currRows2_refs = currRows2.head_rows(headRows);
      arma::mat currRow_refsSummed(1, currRows2_refs.n_cols, fill::zeros);
      if(currRows2_refs.n_rows > 0) {
        currRow_refsSummed = arma::sum(currRows2_refs);
      }

      arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
      toUT = toUT - toUT_refs;
    }

    if(windowForward > 0 && lastRow <= (dfRows-1)) {
      int tail_rows_to_use = lastRow - row;
      if(tail_rows_to_use > 0) {
        arma::mat currRows2_refs = currRows2.tail_rows(tail_rows_to_use);

        arma::mat currRow_refsSummed = arma::sum(currRows2_refs);
        arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
        toUT = toUT - toUT_refs;
      }
    }

    if (binaryStanzas==true) {
      toUT.elem( find(toUT > 0) ).ones();
    }
    df_CoOccurred.row(row) = toUT;
  }
  if(binary == true) {
    df_CoOccurred.elem( find(df_CoOccurred > 0) ).ones();
  }

  return wrap(df_CoOccurred);
}


// @title ref_window_lag
// @name ref_window_lag
// @description TBD
// @param df A dataframe
// @param windowSize Integer for number of rows in the stanza window
// @param binary Logical, treat codes as binary or leave as weighted
//
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
DataFrame ref_window_lag(
    DataFrame df,
    int windowSize = 0,
    bool binary = true
) {
  int dfRows = df.nrows();
  int dfCols = df.size();

  arma::mat df_LagSummed(dfRows, dfCols, fill::zeros);

  arma::mat df_AsMatrix2(dfRows, dfCols, fill::zeros);
  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  for(int row = 0; row < dfRows; row++) {
    arma::mat currRows2 = df_AsMatrix2( span( (row-(windowSize-1)>=0)?(row-(windowSize-1)):0,row ), span::all );
    arma::mat currRowsSummed = arma::sum(currRows2);

    df_LagSummed.row(row) = currRowsSummed;
  }

  return wrap(df_LagSummed);
}

//' Sphere norm
//' @title Sphere norm
//' @description TBD
//' @param dfM Dataframe
//' @export
// [[Rcpp::export]]
NumericMatrix fun_sphere_norm(DataFrame dfM) {
  NumericMatrix m = toNumericMatrix(dfM);

  int rows = m.nrow();
  int cols = m.ncol();
  NumericMatrix output(rows, cols);
  std::fill(output.begin(), output.end(), 0);

  for (int p = 0; p < rows; p++) {
    // Calculate the length of the vector ro  w
    NumericVector squared = Rcpp::pow(m.row(p),2);
    double squaredSum = Rcpp::sum(squared);
    double root = std::sqrt(squaredSum);

    if (root > 0) {
      output.row(p) = ( m.row(p) / root );
    }
  }

  return output;
}

//' Non sphere norm
//'
//' @title Non sphere norm
//' @description TBD
//' @param dfM Dataframe
//' @export
// [[Rcpp::export]]
NumericMatrix fun_skip_sphere_norm(DataFrame dfM) {
  NumericMatrix m = toNumericMatrix(dfM);

  int nrows = m.nrow();
  double largestRowVectorLength = 0;

  for(int rowNum=0; rowNum < nrows; rowNum++) {
    NumericVector squared = Rcpp::pow(m.row(rowNum),2);
    double squaredSum = Rcpp::sum( squared );
    double root = std::sqrt( squaredSum );

    largestRowVectorLength = std::max(largestRowVectorLength, root);
  }
  m = m / largestRowVectorLength;

  return(m);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix center_data_c(arma::mat values) {
  arma::mat centered = values.each_row() - mean(values);
  return Rcpp::wrap(centered);
}

// @title Indices representing an adjacnecey key
// @description Create a matrix of indices representing a co-occurrence
//              adjacency vector.  `len` represents the length of a side in a
//              square matrix.
// @param len Integer
// @param row Which row(s) to return, default to -1, returning both rows. 0
//            returns the top row, 1 will return the bottom row
//
// @return matrix with two rows
// [[Rcpp::export]]
arma::umat triIndices(int len, int row = -1) {
  int vL = len;
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  int s = 0;

  arma::umat vR = arma::umat(2, vS, fill::zeros);
  arma::umat vRone = arma::umat(1, vS, fill::zeros);
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      vR(0, s) = j;
      vR(1, s) = i-1;
      if(row == 0) {
        vRone[s] = j;
      } else if (row == 1) {
        vRone[s] = i -1;
      }
      s++;
    }
  }

  if(row == -1) {
    return vR;
  } else {
    return vRone;
  }
}

// @title Multiobjective, Component by Component, with Ellipsoidal Scaling
// @description [TBD]
// @param adjMats [TBD]
// @param t [TBD]
// @param numDims [TBD]
// [[Rcpp::export]]
Rcpp::List lws_lsq_positions(arma::mat adjMats, arma::mat t, int numDims) { // = R_NilValue ) {
  int upperTriSize = adjMats.n_cols;
  int numNodes = ( pow( ceil(std::sqrt(static_cast<double>(2*upperTriSize))),2) ) - (2*upperTriSize);

  // Weighting matrix, putting half of each line.wieght onto the respective
  // nodes.
  arma::mat weights = arma::mat(adjMats.n_rows, numNodes, fill::zeros);
  int row_count = adjMats.n_rows;
  for (int k = 0; k < row_count; k++) {
    arma::rowvec currAdj = adjMats.row(k);
    int z = 0;
    for(int x = 0; x < numNodes-1; x++) {
      for(int y = 0; y <= x; y++) {
        weights(k,x+1) = weights(k,x+1) + (0.5 * currAdj(z));
        weights(k,y) = weights(k,y) + (0.5 * currAdj(z));
        z = z + 1;
      }
    }
  }

  //row_count = adjMats.n_rows;
  for (int k = 0; k < row_count; k++) {
    double length = 0;
    for(int i = 0; i < numNodes; i++) {
      length = length + std::abs(weights(k,i));
    }
    if(length < 0.0001) {
      length = 0.0001;
    }
    for(int i = 0; i < numNodes; i++) {
      weights(k,i) = weights(k,i) / length;
    }
  }

  arma::mat ssX = arma::mat(numDims, numNodes, fill::zeros);
  arma::mat ssA = weights.t() * weights;
  for(int i = 0; i < numDims; i++) {
    arma::mat ssb = weights.t() * t.col(i);
    ssX.row(i) = arma::solve(ssA, ssb, solve_opts::equilibrate	).t();
  }

  arma::mat centroids = (ssX * weights.t()).t();

  return Rcpp::List::create(
    _("nodes") = ssX.t(), //X.transpose(),
    //_("correlations") = compute_difference_correlations(centroids, t),
    _("centroids") = centroids,
    _("weights") = weights,
    _("points") = t
  );
}
