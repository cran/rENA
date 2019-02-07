//'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// #include <RcppParallel.h>
// using namespace RcppParallel;

NumericMatrix toNumericMatrix_(DataFrame x) {
  int nRows=x.nrows();
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }
  return y;
}

arma::rowvec vector_to_ut(arma::mat v) {
  int vL = v.size();
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  // arma::vec vR( vS, fill::zeros );
  arma::rowvec vR2( vS, fill::zeros );
  int s = 0;
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      // vR[s] = v[j] * v[i-1];
      vR2[s] = v[j] * v[i-1];
      s++;
    }
  }
  return vR2;
}
NumericVector vector_to_ut2(NumericVector v) {
  int vL = v.size();
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  // arma::vec vR( vS, fill::zeros );
  NumericVector vR( vS ); //, fill::zeros );
  int s = 0;
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      // vR[s] = v[j] * v[i-1];
      vR[s] = v[j] * v[i-1];
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
    // df_asNumericMatrix(_,i)=NumericVector(df[i]);
  }


  for(int row = 0; row < dfRows; row++) {
    /**
     * The rows in the current window. CurrentRow + Referrants == windowSize
     */

    // NOTE: change the span to always use 0 if infinite window
    int earliestRow = 0, lastRow = row;

    if (windowSize == std::numeric_limits<double>::infinity()) {
      earliestRow = 0;
    } else if ( row - (windowSize-1) >= 0 ) {
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
      arma::mat currRow_refsSummed = arma::sum(currRows2_refs);

      arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
      toUT = toUT - toUT_refs;
    }
    if(windowForward > 0 && row+windowForward <= (dfRows-1)) {
      arma::mat currRows2_refs = currRows2.tail_rows(windowForward);
      arma::mat currRow_refsSummed = arma::sum(currRows2_refs);
      arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
      toUT = toUT - toUT_refs;
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


//
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

/***R
RcppParallel::setThreadOptions(numThreads = 4)
df = data.frame(a = c(1,2,3,4), b = c(0,0,1,0), c = c(1,1,1,1), d = c(0,1,0,0))
# try_one(rbind(df, df, df, df, df, df), 2)
try_one(df, 2)
*/
