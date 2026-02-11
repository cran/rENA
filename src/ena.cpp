// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>
#include <vector>
#include <ctime>
#include <algorithm>
#include <iterator>
#include <cmath>
#include <RcppArmadillo.h>

//' Fast combn choose 2
//'
//' @param n TBD
//' @description faster combn alternative
//'
//' @export
// [[Rcpp::export]]
arma::umat combn_c2(double n) {
  double n_combos = ( n * ( n - 1 ) ) / 2;
  arma::umat out = arma::zeros<arma::umat>(2, n_combos);

  int col = 0;
  for(int i = 0; i < n_combos; i++) {
    for(int j = i + 1; j < n; j++) {
      out(0, col) = i;
      out(1, col) = j;
      col += 1;
    }
  }

  return(out);
}

//' Calculate the correlations
//'
//' @param points TBD
//' @param centroids TBD
//' @param conf_level TBD
//' @description Calculate both Pearson correlations for the
//' provided points and centorids
//' @export
// [[Rcpp::export]]
arma::mat ena_correlation(arma::mat points, arma::mat centroids, double conf_level = 0.95) {
  arma::umat pComb = combn_c2(points.n_rows);
  arma::umat point1 = pComb.row(0);
  arma::umat point2 = pComb.row(1);

  arma::mat pts_diff = points.rows(point1) - points.rows(point2);
  arma::mat cts_diff = centroids.rows(point1) - centroids.rows(point2);
  arma::mat cor_result = arma::cor(pts_diff, cts_diff);

  Rcpp::NumericVector v = { (1 + conf_level) / 2 };
  Rcpp::NumericVector q = Rcpp::qnorm(v, 0.0, 1.0);
  double qq = q(0);

  arma::mat out(points.n_cols, 3);

  int n = point1.n_cols;
  double r, z, sigma, cint_lower, cint_upper;
  for(arma::uword i = 0; i < points.n_cols; i++) {
    r = cor_result(i,i);
    out(i, 0) = r;

    z = std::atanh(r);
    sigma = 1 / std::sqrt(n - 3);

    cint_lower = z - sigma * qq;
    cint_lower = std::tanh(cint_lower);
    out(i, 1) = cint_lower;

    cint_upper = z + sigma * qq;
    cint_upper = std::tanh(cint_upper);
    out(i, 2) = cint_upper;
  }

  return(out);
}

//' Merge data frame columns
//' @title Merge data frame columns
//' @description TBD
//' @param df Dataframe
//' @param cols Vector
//' @param sep Character seperator
//' @export
// [[Rcpp::export]]
std::vector<std::string> merge_columns_c(
    Rcpp::DataFrame df,
    Rcpp::CharacterVector cols,
    std::string sep = "::"
) {
  int vRows = df.nrows();

  std::vector<std::string> newCol( vRows );

  Rcpp::List colList;
  for (int j = 0; j < cols.length(); j++ ) {
    std::ostringstream oss;
    oss << cols[j];
    std::string col = oss.str();
    Rcpp::CharacterVector cv = df[col];
    colList[col] = cv;
  }

  Rcpp::CharacterVector colNames = colList.names();
  for (int i = 0; i < vRows; i++ ) {
    std::ostringstream ossCol;
    for (int j = 0; j < colNames.length(); j++ ) {
      std::ostringstream oss;
      oss << cols[j];
      std::string colName = oss.str();
      Rcpp::CharacterVector colVec = colList[colName];

      ossCol << colVec[i];
      if(j + 1 < colNames.length()) {
        ossCol << sep;
      }
    }

    newCol[i] = ossCol.str();
  }

  return newCol;
}

Rcpp::NumericMatrix toNumericMatrix(Rcpp::DataFrame x) {
  int nRows=x.nrows();
  Rcpp::NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(Rcpp::_,i)=Rcpp::NumericVector(x[i]);
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

  arma::rowvec vR2( vS, arma::fill::zeros );
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
arma::mat rows_to_co_occurrences(Rcpp::DataFrame df, bool binary = true) {
  int dfRows = df.nrows();
  int dfCols = df.size();
  int numCoOccurences = ( (dfCols * (dfCols + 1)) / 2) - dfCols;

  arma::mat df_AsMatrix2(dfRows, dfCols, arma::fill::zeros);
  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  arma::mat df_CoOccurred(dfRows, numCoOccurences, arma::fill::zeros);
  for(int row = 0; row < dfRows; row++) {
    df_CoOccurred.row(row) = vector_to_ut(df_AsMatrix2.row(row));
  }

  if(binary == true) {
    df_CoOccurred.elem( arma::find(df_CoOccurred > 0) ).ones();
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
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
Rcpp::DataFrame ref_window_df(
    Rcpp::DataFrame df,
    float windowSize = 1,
    float windowForward = 0,
    bool binary = true
  ) {
    //,bool binaryStanzas = false
  int window_back, window_forward;
  int dfRows = (int) df.nrows();
  int dfCols = (int) df.size();
  int numCoOccurences = ( (dfCols * (dfCols + 1)) / 2) - dfCols;

  arma::mat df_CoOccurred(dfRows, numCoOccurences, arma::fill::zeros);
  arma::mat df_AsMatrix2(dfRows, dfCols, arma::fill::zeros);
  // NumericMatrix df_asNumericMatrix(dfRows, dfCols);

  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  double inf = std::numeric_limits<double>::infinity();
  int max = std::numeric_limits<int>::max();
  int min = std::numeric_limits<int>::min();

  if(windowSize == inf || windowSize == max) {
    window_back = max;
  }
  else {
    window_back = windowSize;
  }
  if(windowForward == inf || windowForward == max) {
    window_forward = (int) dfRows; //dfRows;
  }
  else {
    window_forward = (int) windowForward;
  }

  for(int row = 0; row < dfRows; row++) {
    /**
     * The rows in the current window. CurrentRow + Referrants == windowSize
     */

    // NOTE: change the span to always use 0 if infinite window
    int earliestRow = 0, lastRow = row;

    if (window_back == min || window_back == max) {
      earliestRow = 0;
    }
    else if (window_back == 0) {
      earliestRow = row;
    }
    else if ( (row - (window_back-1) >= 0) ) {
      earliestRow = row - (window_back - 1);
    }

    if(window_forward == R_PosInf || (row + (window_forward) >= dfRows)) {
      lastRow = dfRows-1;
    }
    else if ( window_forward > 0 &&  (row + (window_forward) <= dfRows-1)) {
      lastRow = row + window_forward;
    }

    arma::mat currRows2 = df_AsMatrix2( arma::span( earliestRow, lastRow ), arma::span::all );
    arma::mat currRowsSummed = arma::sum(currRows2);
    arma::rowvec toUT = vector_to_ut(currRowsSummed);

    int headRows = 0;
    int currRows2_n_rows = (int) currRows2.n_rows;
    if(currRows2_n_rows > 0 && window_back > 1 && row-1 >= 0) {
      headRows = (int) (currRows2_n_rows - 1 - window_forward);
      if(headRows <= 0) {
        headRows = (int) 0;
      }
      else {
        arma::mat currRows2_refs = currRows2.head_rows(headRows);
        arma::mat currRow_refsSummed(1, currRows2_refs.n_cols, arma::fill::zeros);
        if(currRows2_refs.n_rows > 0) {
          currRow_refsSummed = arma::sum(currRows2_refs);
        }

        arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
        toUT = toUT - toUT_refs;
      }
    }

    if(currRows2_n_rows > 0 && window_forward > 0 && lastRow <= (dfRows-1)) {
      int tail_rows_to_use = lastRow - row;
      if(tail_rows_to_use > 0) {
        arma::mat currRows2_refs = currRows2.tail_rows(tail_rows_to_use);

        arma::mat currRow_refsSummed = arma::sum(currRows2_refs);
        arma::rowvec toUT_refs = vector_to_ut(currRow_refsSummed);
        toUT = toUT - toUT_refs;
      }
    }

    //if (binaryStanzas==true) {
    //  toUT.elem( find(toUT > 0) ).ones();
    //}
    df_CoOccurred.row(row) = toUT;
  }
  if(binary == true) {
    df_CoOccurred.elem( arma::find(df_CoOccurred > 0) ).ones();
  }

  return Rcpp::wrap(df_CoOccurred);
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
Rcpp::DataFrame ref_window_lag(
    Rcpp::DataFrame df,
    int windowSize = 0,
    bool binary = true
) {
  int dfRows = df.nrows();
  int dfCols = df.size();

  arma::mat df_LagSummed(dfRows, dfCols, arma::fill::zeros);

  arma::mat df_AsMatrix2(dfRows, dfCols, arma::fill::zeros);
  for (int i=0; i<dfCols;i++) {
    df_AsMatrix2.col(i) = Rcpp::as<arma::vec>(df[i]);
  }

  for(int row = 0; row < dfRows; row++) {
    arma::mat currRows2 = df_AsMatrix2( arma::span( (row-(windowSize-1)>=0)?(row-(windowSize-1)):0,row ), arma::span::all );
    arma::mat currRowsSummed = arma::sum(currRows2);

    df_LagSummed.row(row) = currRowsSummed;
  }

  return Rcpp::wrap(df_LagSummed);
}

//' Row-wise L2 (Sphere) Normalization
//'
//' @title Row-wise L2 (Sphere) Normalization
//' @description Normalizes each row of a numeric dataframe or matrix to have unit L2 norm (Euclidean length). Each row is divided by its own length, projecting all rows onto the unit hypersphere. Useful for analyses where direction is important but magnitude should be removed.
//' @param dfM A data.frame or matrix. Each row is treated as a vector to compute its L2 norm.
//' @return A numeric matrix with the same dimensions as `dfM`, with each row normalized to unit length (L2 norm = 1), unless the row is all zeros (in which case it remains zeros).
//' @details This function computes the L2 norm (Euclidean length) of each row and divides the row by this value. Rows with zero length are left unchanged.
//' @examples
//' df <- data.frame(a = c(3, 4), b = c(0, 0))
//' fun_sphere_norm(df)
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix fun_sphere_norm(Rcpp::DataFrame dfM) {
  Rcpp::NumericMatrix m = toNumericMatrix(dfM);

  int rows = m.nrow();
  int cols = m.ncol();
  Rcpp::NumericMatrix output(rows, cols);
  std::fill(output.begin(), output.end(), 0);

  for (int p = 0; p < rows; p++) {
    // Calculate the length of the vector ro  w
    Rcpp::NumericVector squared = Rcpp::pow(m.row(p),2);
    double squaredSum = Rcpp::sum(squared);
    double root = std::sqrt(squaredSum);

    if (root > 0) {
      output.row(p) = ( m.row(p) / root );
    }
  }

  return output;
}

//' Row-wise Max-Norm Scaling
//'
//' @title Row-wise Max-Norm Scaling
//' @description Scales all rows of a numeric dataframe by dividing by the largest row vector length (L2 norm) found in the dataframe. This preserves the relative magnitudes between rows but does not normalize each row to unit length. Useful for analyses where relative scale is important but full normalization is not desired.
//' @param dfM A data.frame or matrix. Each row is treated as a vector to compute its L2 norm.
//' @return A numeric matrix with the same dimensions as `dfM`, with all values divided by the largest row L2 norm.
//' @details This function finds the row with the largest L2 norm (Euclidean length) and divides all entries in the matrix by this value. It does not normalize each row individually.
//' @examples
//' df <- data.frame(a = c(3, 4), b = c(0, 0))
//' fun_skip_sphere_norm(df)
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix fun_skip_sphere_norm(Rcpp::DataFrame dfM) {
  Rcpp::NumericMatrix m = toNumericMatrix(dfM);

  int nrows = m.nrow();
  double largestRowVectorLength = 0;

  for(int rowNum=0; rowNum < nrows; rowNum++) {
    Rcpp::NumericVector squared = Rcpp::pow(m.row(rowNum),2);
    double squaredSum = Rcpp::sum( squared );
    double root = std::sqrt( squaredSum );

    largestRowVectorLength = std::max(largestRowVectorLength, root);
  }
  m = m / largestRowVectorLength;

  return(m);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix center_data_c(arma::mat values) {
  arma::mat centered = values.each_row() - arma::mean(values);
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

  arma::umat vR = arma::umat(2, vS, arma::fill::zeros);
  arma::umat vRone = arma::umat(1, vS, arma::fill::zeros);
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
  int numNodes = ( std::pow( std::ceil(std::sqrt(static_cast<double>(2*upperTriSize))), 2.0) ) - (2*upperTriSize);

  // Weighting matrix, putting half of each line.wieght onto the respective
  // nodes.
  arma::mat weights = arma::mat(adjMats.n_rows, numNodes, arma::fill::zeros);
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

  arma::mat ssX = arma::mat(numDims, numNodes, arma::fill::zeros);
  arma::mat ssA = weights.t() * weights;
  for(int i = 0; i < numDims; i++) {
    arma::mat ssb = weights.t() * t.col(i);
    ssX.row(i) = arma::solve(ssA, ssb, arma::solve_opts::equilibrate).t();
  }

  arma::mat centroids = (ssX * weights.t()).t();

  return Rcpp::List::create(
    Rcpp::_("nodes") = ssX.t(), //X.transpose(),
    //Rcpp::_("correlations") = compute_difference_correlations(centroids, t),
    Rcpp::_("centroids") = centroids,
    Rcpp::_("weights") = weights,
    Rcpp::_("points") = t
  );
}


/***
 * Ordered model optimizations
 */

//' Multiobjective, Component by Component, with Ellipsoidal Scaling, for directed ENA
//'
//' @title Multiobjective, Component by Component, with Ellipsoidal Scaling, for directed ENA
//' @description TBD
//' @param line_weights TBD
//' @param points TBD
//' @param numDims TBD
//' @export
// [[Rcpp::export]]
Rcpp::List directed_node_positions(arma::mat line_weights, arma::mat points, int numDims) { //, bool by_column = true) { // = R_NilValue ) {
  int numNodes = std::ceil(std::sqrt(static_cast<double>(line_weights.n_cols)));

  arma::mat node_weights = arma::mat(line_weights.n_rows, numNodes, arma::fill::zeros); // zc: added an extra column

  int row_count = line_weights.n_rows;
  for (int k = 0; k < row_count; k++) {
    arma::mat currAdj = line_weights.row(k);

    int z = 0;
    for(int x = 0; x < numNodes; x++) {
      for(int y = 0; y < numNodes; y++) {
        node_weights(k,x) = node_weights(k,x) + currAdj(z);
        // added the following line, zc, 10.29.2021
        node_weights(k,y) = node_weights(k,y) + currAdj(z);
        z = z + 1;
      }
    }
  }

  for (int k = 0; k < row_count; k++) {
    double length = 0;
    for(int i = 0; i < numNodes; i++) {
      length = length + std::abs(node_weights(k,i));
    }
    if(length < 0.0001) {
      length = 0.0001;
    }
    for(int i = 0; i < numNodes; i++) {
      node_weights(k,i) = node_weights(k,i) / length;
    }
  }

  arma::mat ssX = arma::mat(numDims, numNodes, arma::fill::zeros);
  arma::mat ssA = node_weights.t() * node_weights;
  arma::mat ssb;
  for(int i = 0; i < numDims; i++) {
    ssb = node_weights.t() * points.col(i);
    ssX.row(i) = arma::solve(ssA, ssb, arma::solve_opts::equilibrate  ).t();
  }

  arma::mat centroids = (ssX * node_weights.t()).t();

  return Rcpp::List::create(
    Rcpp::_("nodes") = ssX.t(),
    //Rcpp::_("correlations") = compute_difference_correlations(centroids, t),
    Rcpp::_("centroids") = centroids,
    Rcpp::_("weights") = node_weights, // zc: remember that the last column is all 1
    Rcpp::_("points") = points
  );
}

//' Node position optimization with ground and response weights/points added
//'
//' @title Node position optimization with ground and response weights/points added
//' @description TBD
//' @param line_weights TBD
//' @param points TBD
//' @param numDims TBD
//' @export
// [[Rcpp::export]]
Rcpp::List directed_node_positions_with_ground_response_added(arma::mat line_weights, arma::mat points, int numDims) { //, bool by_column = true) { // = R_NilValue ) {
  int numNodes = std::ceil(std::sqrt(static_cast<double>(line_weights.n_cols)));

  arma::mat node_weights = arma::mat(line_weights.n_rows, numNodes, arma::fill::zeros);

  int row_count = line_weights.n_rows;
  for (int k = 0; k < row_count; k++) {
    arma::mat currAdj = line_weights.row(k);

    int z = 0;
    for(int x = 0; x < numNodes; x++) {
      for(int y = 0; y < numNodes; y++) {
        node_weights(k,x) = node_weights(k,x) + currAdj(z);
        // added the following line, zc, 10.29.2021
        node_weights(k,y) = node_weights(k,y) + currAdj(z);
        z = z + 1;
      }
    }
  }

  for (int k = 0; k < row_count; k++) {
    double length = 0;
    for(int i = 0; i < numNodes; i++) {
      length = length + std::abs(node_weights(k,i));
    }
    if(length < 0.0001) {
      length = 0.0001;
    }
    for(int i = 0; i < numNodes; i++) {
      node_weights(k,i) = node_weights(k,i) / length;
    }
  }
  // the following block is to add ground and response node weights/points
  arma::mat node_weights_added = arma::mat(line_weights.n_rows/2, numNodes, arma::fill::zeros);
  arma::mat points_added = arma::mat(line_weights.n_rows/2, numDims, arma::fill::zeros);

  for(int k=0;k<row_count;k+=2)
  {
    for(int i=0;i<numNodes;i++)
      node_weights_added(k/2,i)=node_weights(k,i)+node_weights(k+1,i);
    for(int i=0;i<numDims;i++)
      points_added(k/2,i)=points(k,i)+points(k+1,i);
  }
  arma::mat ssX = arma::mat(numDims, numNodes, arma::fill::zeros);
  arma::mat ssA = node_weights_added.t() * node_weights_added;
  arma::mat ssb;
  for(int i = 0; i < numDims; i++) {
    ssb = node_weights_added.t() * points_added.col(i);
    ssX.row(i) = arma::solve(ssA, ssb, arma::solve_opts::equilibrate  ).t();
  }

  arma::mat centroids = (ssX * node_weights.t()).t();

  return Rcpp::List::create(
    Rcpp::_("nodes") = ssX.t(),
    //Rcpp::_("correlations") = compute_difference_correlations(centroids, t),
    Rcpp::_("centroids") = centroids,
    Rcpp::_("weights") = node_weights,
    Rcpp::_("points") = points
  );
}


/*** R
# fake_codes_len <- 10;
# fake.codes <- function(x) sample(0:1, fake_codes_len, replace = T)
# codes <- paste("Codes", LETTERS[1:fake_codes_len], sep = "-")
#
# df.units <- data.frame(
#   Name = rep(c("J", "Z"), 6)
# );
# df.conversation <- data.frame(
#   Day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
# )
# df.codes <- data.frame(
#   c1 = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1),
#   c2 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
#   c3 = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
# );
# df <- cbind(df.units, df.conversation);
# df <- cbind(df, df.codes);
# dfDT_codes <- data.table::data.table(df);
#
#
# units.by <- colnames(units);
# convesration.by <- colnames(df.conversation);
# codes <- colnames(df.codes);
#
# initial_cols <- c(units.by, codes)
# just_codes <- c(codes)
#
# vL <- length(codes);
# adjacency.length <- ( (vL * (vL + 1)) / 2) - vL ;
# codedTriNames <- paste("adjacency.code",rep(1:adjacency.length), sep=".");

# df.accum.sep  <- ena.accumulate.data(
#     units = df.units, conversation = df.conversation, codes = df.codes)
# df.accum.inf  <- ena.accumulate.data(
#    units = df.units, conversation = df.conversation, codes = df.codes,
#    window.size.back = Inf)
# print(df.accum.sep$connection.counts)
# print(df.accum.inf$connection.counts)

# accums <- dfDT_codes[,
#   (codedTriNames) := ref_window_df(
#     .SD[, .SD, .SDcols = just_codes],
#     windowSize = 1,
#     windowForward = .Machine$integer.max,
#     binary = TRUE
#   ),
#   by = convesration.by,
#   .SDcols = initial_cols,
#   with = T
# ]
# print(accums)

# accums2 <- dfDT_codes[,
#   (codedTriNames) := ref_window_df(
#     .SD[, .SD, .SDcols = just_codes],
#     windowSize = 5,
#     windowForward = 5,
#     binary = TRUE
#   ),
#   by = convesration.by,
#   .SDcols = initial_cols,
#   with = T
# ]
# print(accums2)

# accums3 <- dfDT_codes[,
#   (codedTriNames) := ref_window_df(
#     .SD[, .SD, .SDcols = just_codes],
#     windowSize = 1,
#     windowForward = .Machine$integer.max,
#     binary = TRUE
#   ),
#   by = convesration.by,
#   .SDcols = initial_cols,
#   with = T
# ]
# print(accums3)

# ena_correlation(as.matrix(set$points)[,1:2], as.matrix(set$model$centroids)[,1:2])
*/
