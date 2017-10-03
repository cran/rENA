###
#' @title ENA Rotate by mean
#'
#' @description Computes a dimensional reduction from a matrix of points such that
#' the first dimension of the projected space passes through the means of two
#' groups in a the original space. Subsequent dimensions of the projected space
#' are computed using ena.svd
#'
#' @param enaset An \code{\link{ENAset}}
#' @param groups A list containing two logical vectors of length \code{nrow(ENA.set$ena.data$units)},
#' where each vector defines whether a unit is in one of the two groups whose means
#' are used to determine the dimensional reduction
#'
#' @export
#' @return \code{\link{ENARotationSet}}
###
ena.rotate.by.mean = function(enaset, groups) {
  groups = list(groups);
  groups = groups[[1]];
  if(length(groups) < 1) return();
  if(!is(groups[[1]], "list")) {
    groups = list(groups);
  }
  data = enaset$line.weights;
  attrData = enaset$enadata$metadata; # attr(data, opts$UNIT_NAMES)

  data = scale(data, scale=F, center=T);

  col = NULL;
  vals = NULL;

  deflated.data = data;
  i = 1;
  weights = matrix(0, nrow = ncol(deflated.data), ncol = length(groups));

  for(group in 1:length(groups)) {
    col = group;
    vals = groups[[group]];

    colOne.vals = deflated.data[vals[[1]],]; #deflated.data[colOne.rows,]
    colTwo.vals = deflated.data[vals[[2]],]; #deflated.data[colTwo.rows,]
    colOne.means = colMeans(colOne.vals)
    colTwo.means = colMeans(colTwo.vals)
    col.mean.diff = colOne.means - colTwo.means

    col.mean.diff.sq = col.mean.diff/sqrt(sum(col.mean.diff^2))

    deflated.data = deflated.data - (deflated.data %*% col.mean.diff.sq) %*% t(col.mean.diff.sq)

    #col.mean.diff.sq = as.matrix(col.mean.diff.sq);
    weights[,i] = col.mean.diff.sq;
    i = i + 1;
  }
  deflated.data.svd = orthogonal.svd(deflated.data, weights); #col.mean.diff.sq);

  colnames(deflated.data.svd) = c(
     paste('V',as.character(1:ncol(deflated.data.svd)), sep='')
  );

  rotationSet = ENARotationSet$new(node.positions=NULL, rotation=deflated.data.svd[,1:2], codes=enaset$codes);
  return(rotationSet);
}

orthogonal.svd = function(data, weights) {
  if(class(data) != "matrix"){
    message("orthogonalSVD:  converting data to matrix")
    data = as.matrix(data)
  }
  #Find the orthogonal transformation that includes W
  Q = qrOrtho(weights)
  X.bar = data%*%Q[,(ncol(weights)+1):ncol(Q)]
  V = prcomp(X.bar, scale.=F)$rotation
  if (class(V)=="numeric") {
    V = matrix(V,nrow=length(V))
  }

  toReturn = (cbind(Q[,1:ncol(weights)], Q[,(ncol(weights)+1):ncol(Q)]%*%V));
  #print(colnames(toReturn))
  return(toReturn);
}

qrOrtho = function(A) {
  return(qr.Q(qr(A),complete=T))
}
