##
# @title EGR Positions
# @description Original ENA position method developed by Epistemic Games
#
#
# @export
##
egr.positions <- function(enaset, checkUnique = F) {
  ###
  # Calculate the rotation distances
  ###
    enaset$rotation_dists = getRotationDistances_c(enaset$points.rotated.non.zero);
  ###

  ###
  # Perform the optimization
  ###
    #OLD
    #enaset$data$optim = enaset$function.params$optim.method(enaset, inPar = enaset$get("inPar"));

    #NEW - just uses temporary local variable
    optim = do_optimization(enaset, inPar = enaset$get("inPar"));
  ###

  ###
  # Store the optimized node positions
  ### - OLD
    #enaset$nodes$positions$optim = get_optimized_node_pos_c(
    #  enaset$line.weights.non.zero, enaset$get("dimensions"), enaset$get("samples"), opted = enaset$data$optim
    #);
  ### - NEW ---
    nodes.positions.optim = get_optimized_node_pos_c(
      enaset$line.weights.non.zero, enaset$get("dimensions"), enaset$get("samples"), opted = optim
    );
    enaset$correlation = nodes.positions.optim["Correlations"];
  ###
  # Store the unscaled node positions
  ### --- OLD
    # enaset$nodes$positions$unscaled = full_opt_c(
    #   normed = enaset$line.weights.non.zero,
    #   rotated = enaset$points.rotated.non.zero,
    #   optim_nodes = nodes.positions.optim,
    #   dims = enaset$get("dimensions"), num_samples = enaset$get("samples")
    #   ,checkUnique = enaset$function.params$check.unique.positions
    # );
    # rownames(enaset$nodes$positions$unscaled$positions) = enaset$enadata$codes;
  ### --- NEW
    nodes.positions.unscaled = full_opt_c(
      normed = enaset$line.weights.non.zero,
      rotated = enaset$points.rotated.non.zero,
      optim_nodes = nodes.positions.optim,
      dims = enaset$get("dimensions"), num_samples = enaset$get("samples")
      ,checkUnique = checkUnique
    );
    rownames(nodes.positions.unscaled$positions) = enaset$enadata$codes;

  ###
  # Scale the node positions
  ###
    enaset$node.positions = full_opt_soln(
      nodes.positions.unscaled$positions,
      enaset$line.weights.non.zero,
      enaset$points.rotated.non.zero
    )$positions;
    rownames(enaset$node.positions) = enaset$enadata$codes;
  ###

  return(enaset);
}
