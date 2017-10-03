sphere.norm=function(m){
  m = as.matrix(m);
  r=nrow(m)
  output=matrix(0,r,ncol(m))
  for (p in 1:r) {
    vlength=(sum(m[p,]^2))^(1/2)
    if (!is.na (vlength)){
      if (vlength>0) {output[p,]=(m[p,]/vlength)}
    }
  }
  output
}
