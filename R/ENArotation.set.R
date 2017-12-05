#######
#' ENARotationSet R6class
#
## Node positions
## Rotation matrix
#
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
#
# @param windowSize Integer used to select the size of each stanza window within a conversation
# @param binary Logical, whether to convert code values to binary or allow for weigthed values
# @param unitsSelected deprecated
#
# @section Public ENARotationSet methods:
#######
ENARotationSet = R6::R6Class("ENARotationSet",
  public = list(

    #######
    ### Constructor - documented in main class declaration
    #######
    initialize = function(
      rotation,
      codes,
      node.positions,
      eigenvalues = NULL
    ) {
      self$node.positions = node.positions;
      self$rotation = rotation;
      self$codes = codes;
      self$eigenvalues = eigenvalues;
    },

    ####
    ## Public Properties
    ####
      rotation = NULL,
      node.positions = NULL,
      codes = NULL,
      eigenvalues = NULL
    ####
    ## END: Public Properties
    ####
  )
)
