full.opt = function(w, t, node_names, return_all=F, dims = 1:6) {
  empties = apply(w, 1, sum) == 0
  w = w[!empties, ]
  t = t[!empties, ]
  return_all = T
  x = get_optimized_node_positions(w,
                                   t[, dims],
                                   node_names=node_names,
                                   num_samples=3,
                                   num_dims=length(dims),
                                   max_iter=1000,
                                   return_all=return_all)
  out = list()
  out$sums_sq_dists = vector(length=length(dims))
  out$x_scaled = matrix(nrow=x$N, ncol=x$num_dims) #, dimnames = list(x$node_names, NULL))

  for(dim in 1:x$num_dims)
  {
    # ::::::: solution check ::::::
    #   throw an error if the solutions aren't linearly dependent
    browser();
    r_sq = lm_(x$x_all_iters[, x$iter_index[, 2] == dim])
    if(min(r_sq) < 0.9)
    {
      message("\nr_sq vals:\n")
      for(irsq in r_sq) message(irsq)
      # stop("\n ill-conditioned solution")
    }

    # ::::::: choose solution with highest correlation ::::::
    #browser()
    highest_corr = which(x$correlations[, dim] == max(x$correlations[, dim]))
    highest_corr = highest_corr + (dim - 1)*x$opt_params$n_samples
    soln = x$x_all_iters[, highest_corr]
    #browser()
    # :::::: remove zero vectors ::::::
    bool = apply(x$w, 1, function(TMP) sum(TMP) != 0)
    result = scale_soln(soln, x$t[bool, dim], x$w[bool, ])

    out$sums_sq_dists[dim] = result$value
    out$x_scaled[, dim] = result$par[1] + result$par[2] * soln
    colnames(out$x_scaled) = colnames(x$t)[1:ncol(out$x_scaled)]
    row.names(out$x_scaled) = x$node_names
  }


  if(!return_all) return(out$x_scaled)

  if(return_all){
    x$x = NULL
    x$x_scaled = out$x_scaled
    x$sums_sq_dists = out$sums_sq_dists
    # x$corr = interpretive.correlation(x = x$x_scaled, t = x$t, w = x$w)

    return(x)
  }
}
