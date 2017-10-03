get_optimized_node_positions = function(w,
                                        t,
                                        node_names=NA,
                                        num_samples=20,
                                        num_dims=2,
                                        max_iter=1000,
                                        return_all=F,
                                        optimMethod = "C", inPar = F)
{
  # :::::: remove zero vectors from w ::::::
  bool = apply(w, 1, function(z) !all(z==0))
  w = w[bool, ]

  # :::::: bundle data into list e ::::::
  e = list()

  e$data.normed = w
  e$data.centered.rotated = t
  e$maxit = max_iter
  e$dims = e$num_dims = num_dims
  e$samples = e$num_samples = num_samples
  e$node_names = node_names
  e$K = getK(e$data.normed)
  e$N = getN(e$data.normed) #0.5+sqrt(0.25+2*ncol(e$data.normed))
  e$i = which(upper.tri(diag(e$N)), arr.ind = T)[, 1]
  e$j = which(upper.tri(diag(e$N)), arr.ind = T)[, 2]
  e$i2 = which(upper.tri(diag(e$K)), arr.ind = T)[, 1]
  e$j2 = which(upper.tri(diag(e$K)), arr.ind = T)[, 2]
  e$rotation_dists = e$data.centered.rotated[e$i2, ] - e$data.centered.rotated[e$j2, ]

  # :::::: Execute all ::::::
  if(optimMethod == "C") {
    y = do_optimization_2(e, inPar)
  } else {
    y = do_optimization(e)
  }

  # :::::: Parse results ::::::
  pc_names = sapply(1:num_dims, function(z) paste(c("PC", z), collapse=""))

  e$Correlations = matrix(y[nrow(y)-1, ],
                          nrow=e$num_samples, ncol=num_dims,
                          dimnames=list(NULL, pc_names))
  e$iter_names = vector(length=ncol(y))
  e$IterIndex = matrix(nrow=e$num_samples*e$num_dims, ncol = 2, dimnames=list(NULL, c("iter", "dim")))
  for(i in 1:ncol(y)){
    e$iter_names[i] = paste(c("PC_", y[nrow(y), i], "_iter_", i), collapse="")
    e$IterIndex[i, 1] = y[nrow(y)-1, i]
    e$IterIndex[i, 2] = y[nrow(y), i]
  }

  e$AllIters = matrix(y[1:(nrow(y)-2), ],
                         nrow=e$N,
                         ncol=num_dims*e$num_samples); #,
                         #dimnames=list(e$node_names,
                        #               e$iter_names))

  e$x = matrix(nrow=e$N, ncol=num_dims); #, dimnames=list(e$node_names, pc_names))
  for(i in 1:num_dims){
    e$x[, i] = rowMeans(y[1:(nrow(y)-2), y[nrow(y), ]==i])
  }

  # :::::: compute centroids and their paired distances ::::::
  e$centroids = matrix(nrow=e$K, ncol=ncol(e$AllIters))
  for(i in 1:ncol(e$centroids)){
    mps = (e$AllIters[e$i, i] + e$AllIters[e$j, i])/2
    e$centroids[, i] = apply(e$data.normed, 1, function(z) sum(z*mps)/sum(z))
  }

  e$cen_pair_dists = e$centroids[e$i2, ] - e$centroids[e$j2, ]

  if(return_all)
  {
    e$opt_params = list(max_iterations = e$maxit,
                        n_samples = e$num_samples,
                        num_dims = e$num_dims)
    e$list_names = names(e)
    for(i in c("maxit",
               "num_samples",
               "i", "j",
               "i2", "j2"))
      e[[i]] = NULL
    return(e)
  }
  if(!return_all) return(e$x)
}
