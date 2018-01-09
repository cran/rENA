##
#' @title Compute summary statistic for groupings of units using given  method (typically, mean)
#'
#' @description Computes summary statistics for groupings (given as vector) of units in ena data using given method (typically, mean); computes summary statistic for point locations and edge weights for each grouping
#'
#' @export
#'
#' @param enaset An \code{\link{ENAset}} (optional)
#' @param by A vector of values the same length as units. Uses rotated points for group positions and normed data to get the group edge weights
#' @param method A function that is used on grouped points. Default: mean()
#'
#' @keywords ENA, set, group
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
#' means = ena.group(set, by=accum$metadata$Condition)
#'
#'
#' @return A list containing names, points, and edge weights for each of the unique groups formed by the function
##
ena.group <- function(
  enaset = NULL,   #ENAset object to form groups from
  by = NULL, #Vector of values  the same length as units.
  method = mean  #method by which to form groups from specified attribute/vector of values
) {
  run.method = function(pts) {
    points.dt = data.table::data.table(pts);
    if(is.logical(by)) {
      points.dt.means = points.dt[by, lapply(.SD,method),]; # by=by];
    } else if(all(by %in% colnames(pts))) {
      points.dt.means = points.dt[, lapply(.SD,method), by=by];
    } else {
      points.dt.means = as.data.frame(aggregate(points.dt, by = list(by), FUN = method)) #"mean"))
      rownames(points.dt.means) = points.dt.means$Group.1
      points.dt.means = points.dt.means[,colnames(points.dt)]
      # agg.df[as.vector(unique(group.by)),]u
      return (points.dt.means[as.vector(unique(by)),]);
    }
    return(as.data.frame(points.dt.means[,colnames(points.dt),with=F]))
  }

  if(is.character(method)) {
    method = get(method)
  }

  if("ENAset" %in% class(enaset)) {
    return(list(
      "names" = as.vector(unique(by)),
      "points" = run.method(enaset$points.rotated),
      "line.weights" = run.method(enaset$line.weights)
    ));
  } else {
    return(run.method(enaset))
  }
}