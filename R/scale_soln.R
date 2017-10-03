# :::::::::::::::::::::::::::::::
# :::::: function scale_soln ::::::
# :::::::::::::::::::::::::::::::
#
#   Linearly transform solution to minimize the sum of
#     centroid-projected point distances
#
# parameters
#   xi - solution on dimension i
#   ti - rotated data on dimension i (set$rotated.data[, i])
#   w - adjacency vectors (set$data)
#
#
# returns
#   list.  optim results.  see help(optim)
#
scale_soln = function(xi, ti, w)
{
  MPS = function(xi) {
    CC = which(upper.tri(diag(length(xi))), arr.ind=T)
    out = (xi[CC[, 1]] + xi[CC[, 2]])/2
    return(out)
  }

  obj = function(coeff, ti, xi) {
    mps = MPS(coeff[1] + coeff[2]*xi)
    ci = apply(w, 1, function(wi) sum(wi*mps)/sum(wi))
    ssd = sum((ci - ti)^2)
    return(ssd)
  }

  out = optim(par = c(1, 1),
              fn = obj,
              control = list(maxit = 100000,
                             reltol=1e-16),
              ti = ti,
              xi = xi)

  return(out)
}
