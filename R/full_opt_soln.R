full_opt_soln = function(unscaled, normed, rotated, dims=2, maxit = 1000) {
  single_optim <- function(dim) {
    result <- suppressWarnings(optim(par = c(1,1),
                                     fn = soln_calc_c,
                                     gr = NULL,
                                     unscaled[, dim], rotated[, dim], normed, dim-1, # ... add'l parameters
                                     method = "Nelder-Mead",
                                     control = list(
                                       reltol=1e-16,
                                       maxit=100000
                                     ),
                                     lower=-1,
                                     upper=1
                                     ));

    return(result)
  }

  optimization_results = matrix(0, nrow=getN(normed), ncol=dims);
  sq_results = c();
  for(dim in 1:dims) {
    res = single_optim(dim);
    optimization_results[,dim] = (res$par[1] + (res$par[2] * unscaled[,dim]));
    sq_results = c(sq_results, res$value);
  }
  return(list( "positions" = optimization_results, "sum_sq_results" = sq_results ));
}
