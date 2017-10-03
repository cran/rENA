get_cor = function(x) {
  dim = get("dim", envir = parent.env(environment()))
  t_pair_dists = e$t_pair_dists[, dim]
  mps = (x[e$i] + x[e$j])/2
  centroids = ((e$w %*% as.matrix(mps)) / rowSums(e$w))[,1]
  dcentroids = centroids[e$i2] - centroids[e$j2]
  return(cor(t_pair_dists, dcentroids))
}
