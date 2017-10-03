###
#' @title ENA SVD
#' @description ENA method computing a dimensional reduction of points in an ENA set using SVD
#' @param enaset An \code{\link{ENAset}}
#' @param ... Unused, necessary for ena.make.set
#' @export
###
ena.svd <- function(enaset, ...) {
  to.norm = data.table::data.table(
    enaset$points.normed.centered,
    merge_columns_c(
      attr(
        enaset$points.normed.centered,
        opts$UNIT_NAMES
      ),
      enaset$enadata$get("units.by")
    )
  )
  to.norm = as.matrix(to.norm[,tail(.SD,n=1),.SDcols=colnames(to.norm)[which(colnames(to.norm) != "V2")],by=c("V2")][,2:ncol(to.norm)]);

  pcaResults = pca_c(to.norm, dims = enaset$get("dimensions"));

  ### used to be  enaset$data$centered$pca
  enaset$rotation.set = pcaResults$pca;
  ### used to be enaset$data$centered$latent
  enaset$variance = pcaResults$latent[enaset$get("dimensions")];

  rotationSet = ENARotationSet$new(rotation = pcaResults$pca, codes = enaset$codes, node.positions = NULL)
  return(rotationSet)
}
