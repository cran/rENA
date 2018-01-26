##
#' @title Generate ENA Set
#'
#' @description Generates an ENA model by constructing a dimensional reduction of adjacency (co-occurrence) vectors in an ENA data object
#'
#' @details This function generates an ENAset object from an ENAdata object. Takes
#' the adjacency (co-occurrence) vectors from enadata, computes a dimensional
#' reduction (projection), and calculates node positions in the projected ENA
#' space. Returns location of the units in the projected space, as well as
#' locations for node positions, and normalized adjacency (co-occurrence) vectors
#' to construct network graphs
#'
#' @export
#'
#' @param enadata \code{\link{ENAdata}} that will be used to generate an ENA model
#' @param dimensions The number of dimensions to include in the dimensional reduction
#' @param norm.by A function to be used to normalize adjacency (co-occurrence) vectors before computing the dimensional reduction, default: sphere_norm_c()
#' @param rotation.by	A function to be used to compute the dimensional reduction, default: ena.svd()
#' @param rotation.params (optional) A character vector containing additional parameters for the function in rotation.by, if needed
#' @param rotation.set A previously-constructed  ENARotationSet object to use for the dimensional reduction
#' @param endpoints.only A logical variable which determines whether to only show endpoints for trajectory models
#' @param node.position.method A function to be used to determine node positions based on the dimensional reduction, default: lws.position.es()
#' @param ... additional parameters addressed in inner function
#'
#' @keywords ENA, generate, set
#'
#' @examples
#' data(RS.data)
#'
#' codeNames = c('Data','Technical.Constraints','Performance.Parameters',
#'   'Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
#'
#' accum = ena.accumulate.data(
#'   units = RS.data[,c("UserName","Condition")],
#'   conversation = RS.data[,c("Condition","GroupName")],
#'   metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post")],
#'   codes = RS.data[,codeNames],
#'   window.size.back = 4
#' )
#'
#' set = ena.make.set(
#'   enadata = accum
#' )
#'
#' set.means.rotated = ena.make.set(
#'   enadata = accum,
#'   rotation.by = ena.rotate.by.mean,
#'   rotation.params = list(
#'       accum$metadata$Condition=="FirstGame",
#'       accum$metadata$Condition=="SecondGame"
#'   )
#' )
#'
#' @seealso \code{\link{ena.accumulate.data}}, \code{\link{ENAset}}
#'
#' @return \code{\link{ENAset}} class object that can be further processed for analysis or plotting
##
ena.make.set <- function(
  enadata,
  dimensions = 2,
  norm.by = sphere_norm_c,
  rotation.by = ena.svd,
  rotation.params = NULL,
  rotation.set = NULL,
  endpoints.only = T,
  node.position.method = lws.positions.sq,
  ...

  # private properties of ENAset
  #dims=2,    #usein in egr.pos/optimization --- to be determined
  #samples=3,   #usein in egr.pos -- make local to egr
  #inPar=F,    #used in egr.pos-- make local to egr

  ####
  #sphere.norm=dont_sphere_norm_c,   #now called norm.by
  #center.data=center_data_c,     ### made local in run - always center_data_c
  #optim.method=do_optimization,  # - now local to ENAset, used by egr.position
  #position.method=egr.positions, #-> node.position.method

  ### what to do with these 2?
  #check.unique.positions=F,
  # set.seed = F,

  ### leaving for now so testing can occur w/o errors
  # rotate.means = F,
  # rotate.means.by = NULL,
  #

  ### NO LONGER BEING INCLUDED
  #output = c("class","json"),
  #output.fields = NULL,

) {
  set = ENAset$new(
    enadata = enadata,

    dimensions = dimensions,
    rotation.by = rotation.by,
    rotation.params = rotation.params,
    rotation.set = rotation.set,
    norm.by = norm.by,
    node.position.method = node.position.method,

    endpoints.only = endpoints.only,

    #### TO BE REMOVED
    # set.seed = set.seed,
    # rotate.means = rotate.means,
    # rotate.means.by = rotate.means.by,
    ####

    ...

  )$process();
  set$function.call = sys.call();

  #output = match.arg(output);

  # if(output == "json") {
  #   output.class = get(class(set))
  #
  #   if(is.null(output.fields)) {
  #     output.fields = names(output.class$public_fields)
  #   }
  #
  #   r6.to.json(set, o.class = output.class, o.fields = output.fields)
  # }
  # else
  return(set)
}
