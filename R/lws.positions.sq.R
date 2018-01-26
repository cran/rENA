##
# @title LWS Least-Square Positions
# @description Least-Squares Position method developed by Jeff Linderoth in collaboration
# with Epistemic Games
#
#
# @export
##
# lws.positions <- function(enaset) {
#   # message("Running positions using the LWS method.");
#
#   positions = linderoth_pos(enaset$line.weights.non.zero, enaset$points.rotated);
#
#   enaset$node.positions = positions$nodes;
#   rownames(enaset$node.positions) = enaset$enadata$codes;
#   return(enaset);
# }

# Ellipsoidal scaling version
lws.positions.sq <- function(enaset) {
  # message("Running positions using the LWS method and ellipsoidal scaling.");
  # positions = linderoth_pos_es(enaset$line.weights, enaset$points.rotated, enaset$get("dimensions"));
  positions = lws_lsq_positions(enaset$line.weights, enaset$points.rotated, enaset$get("dimensions"));

  enaset$node.positions = positions$nodes;
  rownames(enaset$node.positions) = enaset$enadata$codes;

  enaset$centroids = positions$centroids;

  return(enaset);
}
