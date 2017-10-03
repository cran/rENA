
#int numNodes = ( pow(ceil(sqrt(2*upperTriSize)),2) ) - (2*upperTriSize);
make.adj.matrices = function(set, N = ((ceiling(sqrt(2 * ncol(set)))^2) - (2*ncol(set))) ) {
  all.adj.mats = list();

  index = 1;
  apply(set, 1, function(n) {
    b = matrix(0, N, N)
    b[upper.tri(b, diag=F)] = n
    all.adj.mats[[index]] <<-  b
    index <<- index + 1
  })

  all.adj.mats
}
