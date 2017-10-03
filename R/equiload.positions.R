equiload.positions=function(names,labels,vals,plus1) {
  numrows=length(names)

  rotation = vals
  vals = matrix(0, nrow(vals), nrow(vals))
  diag(vals) = 1
  #browser();
  out=matrix(0,numrows,ncol(vals))
  if(plus1 == TRUE) {
    for (p in 1:numrows){
      thisone=names[p]
      q=t(colSums(as.matrix(vals[(labels[1,]==thisone)|(labels[2,]==thisone),])))
      out[p,]=q
    }
    # Multiplying by two is eq to having a matrix of 1s and -1s and adding 1
    out = out * 2
    out = out %*% rotation
    # Scaling
    out = out/(numrows-1)/2
  } else if(plus1 == FALSE) {
    for (p in 1:numrows){
      thisone=names[p]
      q=t(colSums(as.matrix(vals[(labels[1,]==thisone)|(labels[2,]==thisone),])))
      out[p,]=q
    }
    # Multiplying by two is eq to having a matrix of 1s and -1s and adding 1
    out = out %*% rotation
    # Scaling
    out = out/(numrows-1)
  }
  rownames(out)=names
  out
}
