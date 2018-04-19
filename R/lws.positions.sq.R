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
  positions = lws_lsq_positions(enaset$line.weights, enaset$points.rotated, enaset$get("dimensions"));

  node.positions = positions$nodes;
  rownames(node.positions) = enaset$enadata$codes;

  return(list("node.positions" = node.positions, "centroids" = positions$centroids))
}
