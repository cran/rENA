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

int count_if(LogicalVector x) {
  int counter = 0;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      counter++;
    }
  }
  return counter;
}

int vecmin(NumericVector x) {
  // Rcpp supports STL-style iterators
  NumericVector::iterator it = std::min_element(x.begin(), x.end());
  // we want the value so dereference
  return *it;
}

int vecmax(NumericVector x) {
  // Rcpp supports STL-style iterators
  NumericVector::iterator it = std::max_element(x.begin(), x.end());
  // we want the value so dereference
  return *it;
}

#include <sstream>
template <typename T>
std::string NumberToString ( T Number ) {
  std::ostringstream ss;
  ss << Number;
  return ss.str();
}

NumericVector logicalToColNums( LogicalVector lv ) {
  int size = lv.size();
  NumericVector nv(count_if(lv));

  int s=0;
  for ( int i=0; i<size; i++) {
    if(lv[i] == 1) {
      nv(s) = i;
      s++;
    }
  }

  return nv;
}

// [[Rcpp::export]]
NumericVector rowSums_c(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

NumericMatrix toNumericMatrix(DataFrame x) {
  int nRows=x.nrows();
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }
  return y;
}

struct asset_info {
  double sum, sum2, stdev;
};

//[correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence).
// n,sX,sY,sXY,sX2,sY2
// cor = ( n * sXY - sX * sY ) / ( sqrt(n * sX2 - sX^2) * sqrt(n * sY2 - sY^2) )
inline asset_info compute_asset_info(const NumericMatrix& mat,
                                     const int icol, const int rstart, const int rend) {
  double sum, sum2;
  sum = sum2 = 0;

  for (int r = rstart; r < rend; r++) {
    double d = mat(r, icol);
    sum += d;
    sum2 += pow(d,2);
  }

  asset_info res;
  res.sum = sum;
  res.sum2 = sum2;
  res.stdev = std::sqrt( (rend-rstart) * sum2 - pow(sum, 2));
  return res;
}

inline NumericMatrix c_cor_helper(const NumericMatrix& mat, const int rstart, const int rend) {
  int nc = mat.ncol();
  int nperiod = rend - rstart;
  NumericMatrix rmat(nc, nc);

  vector<asset_info> info(nc);
  for (int c = 0; c < nc; c++)
    info[c] = compute_asset_info(mat, c, rstart, rend);

  for (int c1 = 0; c1 < nc; c1++) {
    for (int c2 = 0; c2 < c1; c2++) {
      double sXY = 0;

      for (int r = rstart; r < rend; r++)
        sXY += mat(r, c1) * mat(r, c2);

      rmat(c1, c2) = (nperiod * sXY - info[c1].sum * info[c2].sum) / (info[c1].stdev * info[c2].stdev);
    }
  }

  return rmat;
}

// [[Rcpp::export]]
NumericMatrix c_cor(NumericMatrix mat) {
  return c_cor_helper(mat, 0, mat.nrow());
}

// [[Rcpp::export]]
NumericMatrix sphere_norm_c(DataFrame dfM) {
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

// [[Rcpp::export]]
NumericMatrix dont_sphere_norm_c(DataFrame dfM) {
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
List pca_c(arma::mat m, int dims = 2) {
  arma::mat pca;
  arma::mat score;
  arma::vec latent; // Eigen
  arma::vec tsquared;
  arma::princomp(pca, score, latent, tsquared, m);

  if(pca.n_cols > dims) {
    pca = pca.head_cols(dims);
  }

  return List::create(
    _["pca"] = pca,
    _["latent"] = latent
  );
}


// [[Rcpp::export]]
Rcpp::NumericMatrix center_data_c(arma::mat values) {
  arma::mat centered = values.each_row() - mean(values);
  return Rcpp::wrap(centered);
}

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

// [[Rcpp::export]]
double getcor(
    arma::mat dists, arma::mat normed, arma::mat x,
    arma::uvec NtriOne, arma::uvec NtriTwo,
    arma::uvec KtriOne, arma::uvec KtriTwo,
    int dim = 0
) {
  arma::mat t_pair_dists = dists.col(dim);
  arma::mat mps = ((x(NtriOne) + x(NtriTwo)) / 2);
  arma::mat centroids = ((sum(normed, 0) % trans(mps)) - sum(normed, 0));
  arma::mat dcentroids = centroids(KtriOne); // - centroids(KtriTwo);

  double c = as_scalar(cor(t_pair_dists, dcentroids));

  return(c);
}

// [[Rcpp::export]]
int getN(arma::mat normed) {
  return floor(0.5 + std::sqrt( 0.25 + 2 * normed.n_cols ));
}

// [[Rcpp::export]]
int getK(arma::mat normed) {
  return normed.n_rows;
}

// [[Rcpp::export]]
arma::mat getRotationDistances_c(arma::mat rotated) {
  int K = getK(rotated);
  uvec KtriOne = triIndices(K, 0);
  uvec KtriTwo = triIndices(K, 1);

  arma::mat pairDists = (rotated.rows(KtriOne) - rotated.rows(KtriTwo));

  return pairDists;
}

// [[Rcpp::export]]
Rcpp::List get_optimized_node_pos_c(
    arma::mat normedFiltered,
    NumericMatrix opted,
    int num_dims=2, int num_samples=3, int max_iter=1000,
    bool return_all = true
) {
  arma::uvec b = find(any(normedFiltered != 0, 1) > 0); //apply(normed, 1, function(z) !all(z==0))
  arma::mat normedNonZero = normedFiltered.rows(b);

  double N = getN(normedNonZero);
  int K = getK(normedNonZero);

  uvec NtriOne = triIndices(N, 0);
  uvec NtriTwo = triIndices(N, 1);
  uvec KtriOne = triIndices(K, 0);
  uvec KtriTwo = triIndices(K, 1);

  //arma::mat pairDists = getRotationDistances(rotatedFiltered);
  //NumericMatrix opted = Rcpp::wrap(dataOptim); //do_opt(normedFiltered, pairDists, rotatedFiltered, num_samples, num_dims);
  CharacterVector pc_names(num_dims);
  for(int i=0; i<num_dims; i++) {
    //pc_names[i] = "PC" + std::to_string(i+1);
    pc_names[i] = "PC" + NumberToString(i+1);
  }
  opted.attr("colnames") = pc_names;

  CharacterVector correlationRowNames(num_samples);
  for(int i=0; i<num_samples; i++) {
    correlationRowNames[i] = "Sample " + NumberToString(i+1);
  }

  // Rcpp::Rcout << "Correlations: " << opted << std::endl;
  NumericMatrix correlations = opted( Range(opted.nrow()-2, opted.nrow()-2), _ ); // Range(0,opted.ncol()-num_samples-1) );
  correlations.attr("dim") = Dimension(num_samples,num_dims);

  colnames(correlations) = pc_names;
  rownames(correlations) = correlationRowNames;

  CharacterVector iterNames(opted.ncol());
  NumericMatrix iterIndex(num_dims * num_samples, 2);
  for(int i=0; i<opted.ncol(); i++) {
    iterNames[i] = "PC_" + NumberToString((int) opted(opted.nrow() - 1, i)) + "_iter_"+ NumberToString((int) i);
    iterIndex(i, 0) = opted(opted.nrow() - 2, i);
    iterIndex(i, 1) = opted(opted.nrow() - 1, i);
  }

  arma::mat AllIters = Rcpp::as<arma::mat>(opted);
  AllIters = AllIters.rows(0, AllIters.n_rows - 3);
  arma::mat IterMeans = arma::mat(N, num_dims, fill::zeros);

  NumericVector dimRow = opted( opted.nrow() - 1, _ );

  int t=0;
  for( int i=0; i<num_dims; i++) {
    LogicalVector dimRowFilter = ( dimRow == i+1 );
    arma::vec vecs = Rcpp::as<arma::vec>(logicalToColNums(dimRowFilter));

    arma::mat rowsToSum2 = arma::mat(opted.nrow()-2, num_samples, fill::zeros);
    rowsToSum2 = AllIters(0, vecs(0), size(opted.nrow()-2, vecs.size()));

    arma::vec rowsMeaned = mean(rowsToSum2, 1);

    IterMeans.col( t ) = rowsMeaned; //.col(  );
    t++;
  }

  arma::mat centroids = arma::mat( K, AllIters.n_cols );

  for( int i=0; i<centroids.n_cols; i++ ) {
    arma::mat subbed = AllIters.rows(NtriOne);
    arma::mat subbedTwo = AllIters.rows(NtriTwo);
    arma::vec subbedComb = ( subbed.col(i) + (subbedTwo.col(i) / 2) );
    arma::vec subbedComb2 = normedNonZero * subbedComb;
    arma::vec summedRows = sum(subbedComb2, 1) / sum(normedNonZero, 1);

    centroids.col(i) = summedRows;
  }
  arma::mat centroid_dists = centroids.rows(KtriOne) - centroids.rows(KtriTwo);

  if(return_all == true) {
    //  e$opt_params = list(max_iterations = e$maxit,
    //  n_samples = e$num_samples,
    //  num_dims = e$num_dims)
    //  e$list_names = names(e)

    //  for(i in c("maxit", "num_samples", "i", "j", "i2", "j2"))
    //    e[[i]] = NULL
    return List::create(
      _["centroids"] = centroids,
      _["centorid_dists"] = centroid_dists,
      _["N"] = N,
      _["Correlations"] = correlations,
      _["AllIters"] = AllIters,
      _["IterIndex"] = iterIndex,
      _["means"] = IterMeans
    );
  } else {
    return List::create(
      _["means"] = IterMeans
    );
  }
}

List fastLm(const arma::vec & y, const arma::mat & X) {
  int n = X.n_rows, k = X.n_cols;

  arma::colvec coef = arma::solve(X, y);
  arma::colvec resid = y - X*coef;

  double sig2 = arma::as_scalar(arma::trans(resid)*resid/(n-k));

  arma::colvec stderrest = arma::sqrt(sig2 * arma::diagvec( arma::inv(arma::trans(X)*X)) );
  arma::colvec fitted = X * coef;
  bool intercept = false;

  for(int x=0; x<k; x++) {
    if(all(X.col(x) == X(0,x))) intercept = true;
  }

  return List::create(
    Named("coefficients") = coef,
    Named("stderr")       = stderrest,
    Named("residuals")    = resid,
    Named("fitted.values")= fitted,
    Named("intercept")    = intercept
  );
}

List summary_fastLm_c(List object) {
  arma::vec f = object["fitted.values"];
  arma::vec r = object["residuals"];

  arma::vec mss;
  if (object["intercept"]){
    mss = sum( pow((f - mean(f) ),2) );
  } else {
    mss = sum(pow(f,2));
  }
  arma::vec rss = sum(pow(r,2));

  arma::vec rSquared = mss/(mss + rss);

  return List::create(
    Named("r.squared") = rSquared(0,0)
  );
}

// [[Rcpp::export]]
List lm_(NumericMatrix x) {
  int chose = x.ncol() * (x.ncol() - 1) / 2;
  NumericVector r_sq(chose); //length=choose(ncol(x), 2));

  uvec C1 = triIndices(x.ncol(), 0);
  uvec C2 = triIndices(x.ncol(), 1);
  for( int n = 0; n < C1.size(); n++ ) {
    NumericVector x1nv = x( _, C1[n] );
    NumericMatrix x1nvMat = NumericMatrix( x1nv.size(), 1 );
    x1nvMat( _, 0) = x1nv;
    NumericVector x2nv = x( _, C2[n] );

    arma::vec x1 = Rcpp::as<arma::vec>(x1nv);
    arma::vec x2 = Rcpp::as<arma::vec>(x2nv);

    Rcpp::List lmList = fastLm( x1, x2 ); //x1nvMat, x2nv );

    r_sq[n] = summary_fastLm_c(lmList)["r.squared"];
  }

  return(List::create(
   _["r.squares"] = r_sq
  ));
}

// [[Rcpp::export]]
Rcpp::List full_opt_c(arma::mat normed, arma::mat rotated, Rcpp::List optim_nodes, int dims = 2, int num_samples = 3, bool checkUnique = false) {
  int N = getN(normed);

  NumericMatrix x_unscaled(N, dims);

  NumericMatrix AllIters = Rcpp::as<NumericMatrix>(optim_nodes["AllIters"]);
  NumericMatrix iterIndex = Rcpp::as<NumericMatrix>(optim_nodes["IterIndex"]);
  iterIndex = iterIndex( Range(0, (dims * num_samples) - 1), _ );

  NumericMatrix corrs = optim_nodes["Correlations"];
  NumericVector dimCols = iterIndex( _, 1);

  for( int i=0; i<dims; i++) {
    NumericVector thisDimCols = logicalToColNums(dimCols == (i+1));

    // Check for unique solutions
    if(checkUnique == true) {
      int s=0;
      NumericMatrix AllItersFiltered( AllIters.nrow(), thisDimCols.size());
      for( int j=0; j<thisDimCols.size(); j++) {
        AllItersFiltered( _, s ) = AllIters( _, thisDimCols[j] );
        s++;
      }
      List lm_Result = lm_(AllItersFiltered);
      NumericVector rSquares = lm_Result["r.squares"];
      Rcpp::Rcout << "R squares:" << rSquares << std::endl;
      if(min(rSquares) < 0.9) {
        Rcpp::Rcout << "R squares for dimension "<< i << " indicate ill-conditioned solution: " << rSquares << std::endl;
      }
    }

    NumericMatrix itersForDim = iterIndex( Range(thisDimCols(0), thisDimCols(thisDimCols.size()-1)), _ );
    LogicalVector highest_corr_vec = itersForDim(_, 0) == max( itersForDim(_, 0));
    NumericVector thisDimCols2 = logicalToColNums(highest_corr_vec);

    int highest_corr = thisDimCols(thisDimCols2(0)); // + ((i - 1) * num_samples);
    NumericVector solnV = AllIters( _ , highest_corr);
    x_unscaled(_, i) = solnV;
  }

  return List::create(
    _["positions"] = x_unscaled,
    _["correlations"] = corrs
  );
}


//typedef void (*integrand) (unsigned ndim, const double *x, void *,unsigned fdim, double *fval);
// [[Rcpp::export]]
double calc_cor(
  arma::vec x,
  List set,
  int dim
) {
  arma::mat dists = set["rotation_dists"];

  arma::mat t_pair_dists = dists.col(dim);
  arma::mat normed = set["data.normed"];

  uvec NtriOne = set["n1"];
  uvec NtriTwo = set["n2"];
  uvec KtriOne = set["k1"];
  uvec KtriTwo = set["k2"];

  arma::mat mps = trans(((x(NtriOne) + x(NtriTwo)) / 2));
  arma::mat multRes = normed * mps.t();
  arma::mat centroids = multRes / sum(normed, 1);
  arma::mat dcentroids = centroids(KtriOne) - centroids(KtriTwo);

  arma::mat cmat2(t_pair_dists.n_rows, 2);
  cmat2.col(0) = t_pair_dists;
  cmat2.col(1) = dcentroids;

  NumericMatrix cc = c_cor(Rcpp::wrap(cmat2));
  //arma::mat c = cor(t_pair_dists, dcentroids, 0);

  //Rcpp::Rcout << "Cor 1: " << c(0,0) << std::endl;
  //Rcpp::Rcout << "Cor 2: " << cc(1,0) << std::endl;
  return cc(1,0);
  //return c(0,0);
}

// [[Rcpp::export]]
arma::vec soln_MPS(arma::mat x) {
  arma::rowvec xVec = x.t();
  arma::uvec CC1 = triIndices(xVec.size(), 0);
  arma::uvec CC2 = triIndices(xVec.size(), 1);
  arma::vec mps = ((xVec(CC1) + xVec(CC2)) / 2);
  return(mps);
}

// [[Rcpp::export]]
double soln_calc_c(
  arma::vec coeff, arma::vec xi, arma::vec ti, arma::mat w, int dim
) {
  arma::vec mps = soln_MPS( (coeff[0]) + ( (coeff[1]) * xi) );

  arma::vec ci = arma::vec(w.n_rows);
  for(int i=0; i < w.n_rows; i++) {
    ci[i] = sum(w.row(i) * mps) / sum(w.row(i)) ;
  }

  double ssd = sum(arma::pow((ci - ti), 2));
  return(ssd);
}

// [[Rcpp::export]]
arma::mat remove_zero_rows_c(arma::mat toFilter) {
  arma::uvec b = find(any(toFilter != 0, 1) > 0);
  arma::mat filtered = toFilter.rows(b);
  return(filtered);
}

// [[Rcpp::export]]
arma::mat remove_zero_rows_by_c(arma::mat toFilter, arma::mat indices) {
  arma::uvec b = find(any(indices != 0, 1) > 0);
  arma::mat filtered = toFilter.rows(b);
  return(filtered);
}


// bool do_opt_c(Rcpp::List set, int maxIterations, Function optim, NumericVector nv) {
//   Rcpp::Rcout << "Optim: " << optim << std::endl;
//   SEXP result=optim(_["par"]=nv, _["fn"]=calc_cor,  _["e"]=set, _["dim"]=1, _["lower"]=-3, _["upper"]=3);
//   Rcpp::Rcout << "Optim res: " << result << std::endl;
//   return true;
// }
