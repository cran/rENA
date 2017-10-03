make.adjacency.labels=function(x){
  y=length(x)
  z=matrix(NA, 4, y * (y - 1) / 2)
  col=0
  for (p in 2:y) {
    for (q in 1:(p-1)) {
      col=col+1;
      z[1,col]=x[p]
      z[2,col]=x[q]
      z[3,col]=p
      z[4,col]=q
    }
  }
  z
}
