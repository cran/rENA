# parameters
#   x - n by m matrix of node positions where n
#       is the number of nodes and m is the number of solutions in the sample
#
# returns
#   vector of r_squared values from linear regressions of one column from x on another
# details
#   the columns of x are supposed to be linearly dependent so the r_squared values should all be near 1
lm_ = function(x) {
  r_sq = vector(length=choose(ncol(x), 2))
  C = which(upper.tri(diag(ncol(x))), arr.ind=T)
  for(n in 1:nrow(C))
  {
    i = C[n, 1]
    j = C[n, 2]
    r_sq[n] = summary(lm(x[, i] ~ x[, j]))$r.squared
  }

  return(r_sq^2)
}
