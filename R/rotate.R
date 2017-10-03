rotate=function(m){
  m = t(t(m) - colMeans(m));
  m=as.matrix(m)

  #print("running prcomp")
  n=prcomp(m, retx=FALSE,scale=FALSE,center=FALSE, tol=0)
  #browser()
  colnames(n$rotation) <- sub("PC", "svd ", colnames(n$rotation))
  n$eigenvalues=n$sdev^2
  n$percent=n$eigenvalues/sum(n$eigenvalues)

  dimensions = 1:6
  dimensions = dimensions[!(dimensions > ncol(n$rotation))]
  n$rotation = n$rotation[, dimensions]
  n$eigenvalues=n$eigenvalues[dimensions]
  n$percents=n$percents[dimensions]
  return (n)
}
