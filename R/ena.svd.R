#' @title ENA SVD
#' @description Computes a dimensional reduction of points in an ENA set using Singular Value Decomposition (SVD).
#' @param enaset An \code{ENAset} object containing the points to be reduced.
#' @param params A list of parameters. Use \code{params$as_object = TRUE} to return an ENARotationSet object, or \code{FALSE} (default) to return a list.
#' @details This function computes the SVD of the points in the ENA set and returns either an ENARotationSet object or a list with the rotation matrix, codes, node positions, and eigenvalues, depending on \code{params$as_object}.
#' @return An ENARotationSet object or a list containing:
#'   \item{rotation}{The rotation matrix from SVD}
#'   \item{codes}{The code names used for the matrix}
#'   \item{node.positions}{(Currently NULL) Node positions}
#'   \item{eigenvalues}{The eigenvalues (squared singular values) from SVD}
#' @examples
#' data(RS.data)
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   model()
#' # SVD as list:
#' svd_result <- ena.svd(enaset, list(as_object = FALSE))
#' # SVD as ENARotationSet object:
#' svd_obj <- ena.svd(enaset, list(as_object = TRUE))
#' @export
ena.svd <- function(enaset, params) {
  # to.norm = data.table::data.table(
  #   enaset$points.normed.centered,
  #   enaset$enadata$unit.names
  # )
  # to.norm = as.matrix(to.norm[,tail(.SD,n=1),.SDcols=colnames(to.norm)[which(colnames(to.norm) != "V2")],by=c("V2")][,2:ncol(to.norm)]);
  # pcaResults = pca_c(to.norm, dims = enaset$get("dimensions"));
  # pcaResults = pca_c(enaset$points.normed.centered, dims = enaset$get("dimensions"));
  as_object = FALSE;
  if(!is.null(params$as_object)) {
    as_object = params$as_object
  }

  # pts = enaset$model$points.for.projection[,!colnames(enaset$model$points.for.projection) %in% colnames(enaset$meta.data), with=F]
  pts = as.matrix(enaset$model$points.for.projection)
  pcaResults = prcomp(pts, retx=FALSE, scale=FALSE, center=FALSE, tol=0)

  ### used to be  enaset$data$centered$pca
  #enaset$rotation.set = pcaResults$pca;


  colnames(pcaResults$rotation) = c(
    paste('SVD',as.character(1:ncol(pcaResults$rotation)), sep='')
  );

  # rotationSet = ENARotationSet$new(rotation = pcaResults$pca, codes = enaset$codes, node.positions = NULL, eigenvalues = pcaResults$latent)
  if(isTRUE(as_object)) {
    rotationSet = ENARotationSet$new(
      rotation = pcaResults$rotation,
      codes = enaset$rotation$codes,
      node.positions = NULL,
      eigenvalues = pcaResults$sdev^2
    )
  }
  else {
    rotationSet <- list(
      rotation = pcaResults$rotation,
      codes = enaset$rotation$codes,
      node.positions = NULL,
      eigenvalues = pcaResults$sdev^2
    )
  }
  return(rotationSet)
}

ena.svd.R6 <- function(enaset, ...) {
  pcaResults = prcomp(enaset$points.normed.centered, retx=FALSE,scale=FALSE,center=FALSE, tol=0)

  colnames(pcaResults$rotation) = c(
    paste('SVD',as.character(1:ncol(pcaResults$rotation)), sep='')
  );

  rotationSet = ENARotationSet$new(
    rotation = pcaResults$rotation, codes = enaset$codes,
    node.positions = NULL, eigenvalues = pcaResults$sdev^2
  )
  return(rotationSet)
}
