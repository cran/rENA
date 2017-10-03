#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace Eigen;
using Eigen::Map;         // 'maps' rather than copies
using Eigen::Matrix;
using Eigen::MatrixXd;
using Eigen::MatrixXcd;   // variable size matrix, double precision
using Eigen::VectorXd;    // variable size vector, double precision
using Eigen::Vector3d;    // variable size vector, double precision
using Eigen::EigenSolver; // one of the eigenvalue solvers

class Offset {
private:
  int nrows, ncols; //, nmats ;

public:
  Offset( int nrows_, int ncols_ ) : //, int nmats_) :
    nrows(nrows_),
    ncols(ncols_)
    // ,nmats(nmats_)
  {
  }

  int operator()( int i, int j, int k){
    return i + j * nrows + k * ( nrows * ncols ) ;
  }

} ;

//MatrixXcd
// [[Rcpp::export]]
Rcpp::List linderoth_pos(Eigen::MatrixXd adjMats, Eigen::MatrixXd t) { // = R_NilValue ) {
  int upperTriSize = adjMats.cols();
  int numNodes = ( pow(ceil(sqrt(2*upperTriSize)),2) ) - (2*upperTriSize);
  int numDims = 2;

  // Rcpp::Rcout << "Num nodes: " << numNodes << std::endl;

  // MatrixXd M = adjMats;
  // MatrixXd N = M.transpose() * M;
  //Rcpp::Rcout << "N: " << N << std::endl;

  // if(t == NULL) {
  //   EigenSolver<MatrixXd> es(N);
  //   Eigen::MatrixXcd evs; // = es.eigenvectors();
  //   evs.resize(N.rows(), 2);
  //   VectorXcd evals = es.eigenvalues();
  //
  //   int indexOne = 0, indexTwo = 0, evalLength = evals.size();
  //
  //   for(int i = 0; i < evalLength; i++) {
  //     if(evals(i).real() > evals(indexOne).real()) {
  //       indexTwo = indexOne;
  //       indexOne = i;
  //     } else if (evals[i].real() > evals[indexTwo].real()) {
  //       indexTwo = i;
  //     }
  //   }
  //   evs.col(0) = es.eigenvectors().col(indexOne);
  //   evs.col(1) = es.eigenvectors().col(indexTwo);
  //
  //   MatrixXcd t = M * evs; //.leftCols(numDims);
  // }
  // Rcpp::Rcout << "t: " << t << std::endl;

  MatrixXd weights = MatrixXd::Zero(adjMats.rows(), numNodes);
  for (int k = 0; k < adjMats.rows(); k++) {
    VectorXd currAdj = adjMats.row(k);
    int z = 0; // adjIndex
    for(int x = 0; x < numNodes-1; x++) {
      for(int y = 0; y <= x; y++) {
        weights(k,x+1) = weights(k,x+1) + (0.5*currAdj(z));
        weights(k,y) = weights(k,y) + (0.5 * currAdj(z));
        z = z + 1;
      }
    }
  }
  // Rcpp::Rcout << "weights: " << weights << std::endl;
  for (int k = 0; k < adjMats.rows(); k++) {
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
  // Rcpp::Rcout << "weights: " << weights << std::endl;

  Rcpp::NumericVector delta = Rcpp::NumericVector ( Rcpp::Dimension(adjMats.rows(), adjMats.rows(), numDims) );
  Offset offset( adjMats.rows(), adjMats.rows() ); //, numDims ) ;
  for(int a = 0; a < adjMats.rows(); a++) {
    for(int b = 0; b < adjMats.rows(); b++) {
      for(int m = 0; m < numDims; m++) {
        delta[ offset(a,b,m) ] = (t(a,m) - t(b,m)) ; //MatrixXd::Zero(M.cols(), numDims);
      }
    }
  }
  // Rcpp::Rcout << "delta: " << delta << std::endl;

  MatrixXd gamma = MatrixXd::Zero(numDims, numNodes);
  for(int i = 0; i < numDims; i++) {
    for (int j = 0; j < numNodes; j++) {
      for(int a = 0; a < adjMats.rows(); a++) {
        for(int b = 0; b < adjMats.rows(); b++) {
          gamma(i,j) += delta[ offset(a,b,i) ] * ( weights(a,j) - weights(b,j) );
        }
      }
    }
  }
  // Rcpp::Rcout << "Gamma: " << gamma << std::endl;

  MatrixXd X = MatrixXd::Zero(numDims, numNodes);
  for(int i = 0; i < numDims; i++) {
    for (int j = 0; j < numNodes; j++) {
      X(i,j) = gamma(i,j) / gamma.row(i).norm();
    }
  }
  // Rcpp::Rcout << "X: " << X << std::endl;

  MatrixXd centroids = (X * weights.transpose()).transpose();
  double totalcorr = 0.0;
  for(int a = 0; a < adjMats.rows(); a++) {
    for(int b = 0; b < adjMats.rows(); b++) {
      if(a != b) {
        double n1 = (t.row(a) - t.row(b)).norm();
        double n2 = (centroids.row(a) - centroids.row(b)).norm();

        VectorXd numer1 = (t.row(a) - t.row(b)).real();
        VectorXd numer2 = (centroids.row(a) - centroids.row(b));

        double numer = numer1.dot(numer2);
        double denom = n1*n2;
        double corr = numer / denom;
        totalcorr += corr;
      }
    }
  }

  double alphanum = 0 , alphadom = 0;
  for(int k = 0; k < adjMats.rows(); k++) {
    for(int i = 0; i < numDims; i++) {
      alphanum += t(k,i) * centroids(k,i);
      alphadom += centroids(k,i) * centroids(k,i);
    }
  }
  double alpha = alphanum / alphadom;

  X = alpha * X;
  centroids = alpha * centroids;
  // Rcpp::Rcout << "X: " << X << std::endl;
  // Rcpp::Rcout << "C: " << centroids << std::endl;

  return Rcpp::List::create(
    _("nodes") = X.transpose(),
    _("points") = t
  );
}

/*** R
#linderoth_pos(4, enaset$line.weights)
#linderoth_pos(enaset$line.weights[1,4])
#linderoth_pos(testAdjMatsTris)
# out = linderoth_pos(enasetNewcomb$line.weights)
*/
