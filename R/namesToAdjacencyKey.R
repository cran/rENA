##
#' @title Names to Adjacency Key
#'
#' @description Convert a vector of strings, representing names of a square matrix, to an adjacency
#'
#' @details Returns a matrix of 2 rows by choose(length(vector), 2) columns
#'
#' @export
#'
#' @param vector Vector representing the names of a square matrix
##
namesToAdjacencyKey <- function(vector) {
  upperTriIndices = triIndices(length(vector)) + 1;
  matrix(vector[upperTriIndices], nrow=2)
}
