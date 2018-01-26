###
#' @title ENA SVD
#' @description ENA method computing a dimensional reduction of points in an ENA set using SVD
#' @param enaset An \code{\link{ENAset}}
#' @param ... Unused, necessary for ena.make.set
#' @export
###
ena.svd <- function(enaset, ...) {
  # to.norm = data.table::data.table(
  #   enaset$points.normed.centered,
  #   enaset$enadata$unit.names
  # )
  # to.norm = as.matrix(to.norm[,tail(.SD,n=1),.SDcols=colnames(to.norm)[which(colnames(to.norm) != "V2")],by=c("V2")][,2:ncol(to.norm)]);
  # pcaResults = pca_c(to.norm, dims = enaset$get("dimensions"));
  # pcaResults = pca_c(enaset$points.normed.centered, dims = enaset$get("dimensions"));
  pcaResults = prcomp(enaset$points.normed.centered, retx=FALSE,scale=FALSE,center=FALSE, tol=0)

  ### used to be  enaset$data$centered$pca
  enaset$rotation.set = pcaResults$pca;

  # rotationSet = ENARotationSet$new(rotation = pcaResults$pca, codes = enaset$codes, node.positions = NULL, eigenvalues = pcaResults$latent)
  rotationSet = ENARotationSet$new(rotation = pcaResults$rotation, codes = enaset$codes, node.positions = NULL, eigenvalues = pcaResults$sdev^2)
  return(rotationSet)
}
