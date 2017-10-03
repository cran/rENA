do_optimization = function(e, inPar = F, maxit = 1000)
{
  e_ = e;

  if(is(e, "ENAset")) {
    e_list = list(
      data.normed = e$line.weights.non.zero,
      rotation_dists = e$rotation_dists,
      dims = e$get("dimensions"),
      samples = e$get("samples")
    );

    e = e_list;
  }
  # :::::: load and register doParallel ::::::
  registerDoParallel(cores = detectCores())

  # :::::: function single_optim ::::::
  single_optim = function(e, dim) {

    get_cor = function(x) {
      dim = get("dim", envir = parent.env(environment()))
      t_pair_dists = e$rotation_dists[, dim]
      mps = (x[e$i] + x[e$j])/2
      centroids = ((e$data.normed %*% as.matrix(mps)) / rowSums(e$data.normed))[,1]
      dcentroids = centroids[e$i2] - centroids[e$j2]
      return(cor(t_pair_dists, dcentroids))
    }
    if(e_$get("set.seed") != F) {
      set.seed(e_$get("set.seed"));
    }
    suppressWarnings(result <- optim(par = runif(e$N,-3, 3),
                                     fn = get_cor,
                                     method = "Nelder-Mead",
                                     control = list(fnscale=-1,
                                                    maxit=maxit),
                                     lower=-3,
                                     upper=3))
    out = c(result$par, result$value, dim)
    #names(out) = c(e$node_names, "corr", "dim")
    return(out)
  }

  e$N = e_$get('N'); #getN(e$data.normed);
  e$K = e_$get('K'); #getK(e$data.normed);
  e$i = e_$get('n1'); #which(upper.tri(diag(e$N)), arr.ind = T)[, 1]
  e$j = e_$get('n2'); #which(upper.tri(diag(e$N)), arr.ind = T)[, 2]
  e$i2 = e_$get('k1'); #which(upper.tri(diag(e$K)), arr.ind = T)[, 1]
  e$j2 = e_$get('k2'); #which(upper.tri(diag(e$K)), arr.ind = T)[, 2]

  if(inPar == T) {
    # :::::: execute in parallel ::::::
    optimization_results=
      foreach(dim=1:e$dims, .combine=cbind) %:%
      foreach(i_sample=1:e$samples, .combine=cbind) %do% {
        single_optim(e, dim)
      }
  } else {
    optimization_results=matrix(0, nrow=(getN(e$data.normed)+2), ncol=e$dims*e$samples);
    col = 1;
    for(dim in 1:e$dims) {
      for(i_sample in 1:e$samples) {
        optimization_results[,col] = single_optim(e, dim);
        col = col + 1;
      }
    }
  }
  return(optimization_results)
}

do_optimization_2 = function(e, inPar=F, maxit = 1000) {
  e_ = e;

  if(is(e, "ENAset")) {
    e_list = list(
      data.normed = e$line.weights.non.zero,
      rotation_dists = e$rotation_dists,
      dims = e$get("dimensions"),
      samples = e$get("samples"),
      N = e_$get('N'),
      K = e_$get('K'),

      ###
      # These indices have been increased from the initial C++
      # calculations for use as R indices. Decrease all of them
      # for safe use in C++ again.
      ###
       n1 = e_$get('n1') - 1,
       n2 = e_$get('n2') - 1,
       k1 = e_$get('k1') - 1,
       k2 = e_$get('k2') - 1
    );

    e = e_list;
  }
  # :::::: function single_optim ::::::
  limits=list(min=-3,max=3);

  single_optim_2 = function(e, dim, N = getN(e$data.normed)) {
    if(e_$get("set.seed") != F) {
      set.seed(e_$get("set.seed"));
    }
    result <- suppressWarnings(optim(par = runif(N,limits$min, limits$max),
                                   fn = calc_cor,
                                     gr = NULL,
                                     e, dim-1, # ... parameters to calc_cor()
                                     method = "Nelder-Mead",
                                     control = list(
                                       fnscale=-1, maxit=1000
                                     )
                                     ,lower=limits$min, upper=limits$max))
    out = c(result$par, result$value, dim)
    return(out)
  }

  N = getN(e$data.normed);
  if(inPar == T) {
    registerDoParallel(cores = detectCores()-1)

    # :::::: execute in parallel ::::::
    optimization_results=
      foreach(dim=1:e$dims, .combine=cbind) %:%
      foreach(i_sample=1:e$samples, .combine=cbind) %do% {
        single_optim_2(e, dim, N)
      }
  } else {
    optimization_results=matrix(0, nrow=(N+2), ncol=e$dims*e$samples);
    col = 1;
    for(dim in 1:e$dims) {
      for(i_sample in 1:e$samples) {
        optimization_results[,col] = single_optim_2(e, dim, N);
        col = col + 1;
      }
    }
  }

  return(optimization_results)
}
