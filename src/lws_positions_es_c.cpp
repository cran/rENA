#include <RcppEigen.h>
#include <typeinfo>

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
  {
  }

  int operator()( int i, int j, int k){
    return i + j * nrows + k * ( nrows * ncols ) ;
  }

} ;

VectorXd compute_difference_correlations(MatrixXd c, MatrixXd t) {
  int K = c.rows();
  int M = c.cols();

  VectorXd corrs = VectorXd::Zero(M);
  for (int i = 0; i < M; i++) {
    double dlen = 0, clen = 0, numerator = 0, corr;

    for (int a = 0; a < K; a++) {
      for (int b = 0; b < K; b++) {
        double dabi = t(a,i) - t(b,i);
        double cabi = c(a,i) - c(b,i);
        numerator += dabi * cabi;
        dlen += (dabi*dabi);
        clen += (cabi*cabi);
      }
    }

    corr = numerator / (sqrt(dlen)*sqrt(clen));
    corrs[i] = corr;
  }

  return(corrs);
}

double component_norm(MatrixXd w, VectorXd t, VectorXd x) {
  int K = w.rows();

  double norm = 0;
  for(int k = 0; k < K; k++) {
    RowVectorXd Krow = w.row(k);
    double ckMtk = x.dot(Krow) - t(k);
    norm += ckMtk * ckMtk;
  }

  return(sqrt(norm));
}

// @title Multiobjective, Component by Component, with Ellipsoidal Scaling
// @description [TBD]
// @param adjMats [TBD]
// @param t [TBD]
// [[Rcpp::export]]
Rcpp::List linderoth_pos_es(Eigen::MatrixXd adjMats, Eigen::MatrixXd t) { // = R_NilValue ) {
  int upperTriSize = adjMats.cols();
  int numNodes = ( pow(ceil(sqrt(2*upperTriSize)),2) ) - (2*upperTriSize);
  int numDims = 2;

  MatrixXd weights = MatrixXd::Zero(adjMats.rows(), numNodes);
  for (int k = 0; k < adjMats.rows(); k++) {
    VectorXd currAdj = adjMats.row(k);
    int z = 0;
    for(int x = 0; x < numNodes-1; x++) {
      for(int y = 0; y <= x; y++) {
        weights(k,x+1) = weights(k,x+1) + (0.5 * currAdj(z));
        weights(k,y) = weights(k,y) + (0.5 * currAdj(z));
        z = z + 1;
      }
    }
  }

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

  MatrixXd sigma = MatrixXd::Zero(numNodes, numNodes);
  VectorXd q;
  for( int k = 0; k < adjMats.rows(); k++) {
    for( int j = 0; j < adjMats.rows(); j++) {
      q = weights.row(k) - weights.row(j);
      sigma = sigma + ( q*q.transpose() );
    }
  }

  EigenSolver<MatrixXd> es(sigma);
  Eigen::MatrixXcd evs;
  evs.resize(sigma.rows(), 2);
  VectorXcd evals = es.eigenvalues();

  NumericVector evalsVec = Rcpp::wrap(evals);
  if(min(evalsVec) < 0.001) {
    Rcpp::Environment base("package:base");
    Rcpp::Function message_r = base["message"];
    //message_r("Warning: Sigma not positive definite. Adding 0.1 to diagonal");

    sigma = sigma + (0.1 * MatrixXd::Identity(numNodes, numNodes));
  }

  MatrixXd sigmaInver = sigma.inverse();

  MatrixXd X = MatrixXd::Zero(numDims, numNodes);
  for( int i = 0; i < numDims; i++) {
    MatrixXd delta = MatrixXd::Zero(adjMats.rows(), adjMats.rows());

    for (int a = 0; a < adjMats.rows(); a++) {
      for (int b = 0; b < adjMats.rows(); b++) {
        delta(a,b) = t(a,i) - t(b,i);
      }
    }

    double R = delta.norm();

    VectorXd gamma = VectorXd::Zero(numNodes);
    for( int j = 0; j < numNodes; j++) {
      for (int a = 0; a < adjMats.rows(); a++) {
        for (int b = 0; b < adjMats.rows(); b++) {
          gamma[j] += delta(a,b) * (weights(a,j) - weights(b,j));
        }
      }
    }

    RowVectorXd x = sigmaInver * gamma;

    VectorXd alphaV = (gamma * x);
    double alphaDblNew = gamma.dot(x);
    alphaDblNew = pow(alphaDblNew, 0.5);
    double alpha = R / alphaDblNew;
    x = alpha * x;

    // double dist = component_norm(weights, t.col(i), x);
    double sumck = 0, sumtk = 0, sumcksq = 0, sumcktk = 0;

    for (int k = 0; k < adjMats.rows(); k++) {
      RowVectorXd wrow = weights.row(k);
      double ck = wrow.dot(x);
      double tk = t(k,i);
      sumck += ck;
      sumcksq += ck*ck;
      sumtk += tk;
      sumcktk += ck*tk;
    }

    double det = adjMats.rows()*sumcksq - (sumck*sumck);

    alpha = (1/det) * ((adjMats.rows()*sumcktk) - (sumck*sumtk));
    double beta = (1/det) * ((-sumck*sumcktk) + (sumtk*sumcksq));
    x = (x * alpha);
    x = x.array() + beta;

    X.row(i) = x;
  }

  MatrixXd centroids = (X * weights.transpose()).transpose();

  return Rcpp::List::create(
    _("nodes") = X.transpose(),
    _("correlations") = compute_difference_correlations(centroids, t),
    _("centroids") = centroids,
    _("weights") = weights,
    _("points") = t
  );
}

/*** R
#linderoth_pos(4, enaset$line.weights)
#linderoth_pos(enaset$line.weights[1,4])
#linderoth_pos(testAdjMatsTris)
# df.file <- system.file("extdata", "rs.data.csv", package="rENA")
# codeNames = c("E.data","S.data","E.design","S.design","S.professional","E.client","V.client","E.consultant","V.consultant","S.collaboration","I.engineer","I.intern","K.actuator","K.rom","K.materials","K.power");
# df.accum = ena.accumulate.data(
#   df.file, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"),
#   code.names = codeNames,
#   trajectory.by = c("ActivityNumber"), trajectory.type = "accumulated"
# );
# df.set.lws = ena.make.set(df.accum, position.method = lws.positions)
# out = linderoth_pos_es(df.set.lws$line.weights, df.set.lws$points.rotated)
# out = linderoth_pos_es(adjMatrix, rotMatrix)
*/
