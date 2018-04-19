#include <typeinfo>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace arma;

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
  for (int k = 0; k < adjMats.n_rows; k++) {
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

  for (int k = 0; k < adjMats.n_rows; k++) {
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
# out = lws_lsq_positions(gend$set$line.weights, gend$set$points.rotated, 2)
# out2= rENA:::linderoth_pos_es(gend$set$line.weights, gend$set$points.rotated, 2)

# out = linderoth_pos_es(adjMatrix, rotMatrix)
*/
