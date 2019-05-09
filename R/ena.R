### ENA Wrapper Function ###

##
#' @title TBD
#'
#' @description TBD
#'
#' @details TBD
#'
#' @export
#'
#' @param data A data frame
#' @param units Column names of data by which units will be identified
#' @param conversation Column names of data by which conversations will be identified
#' @param codes Column names of data by which codes will be identified
#' @param metadata (optional) Column names of data identifying metadata to be associated with each unit in the data
#' @param model A character, choices: EndPoint (or E), AccumulatedTrajectory (or A), or SeparateTrajectory (or S); default: EndPoint. Determines the ENA model to be constructed
#' @param weight.by (optional) A function to apply to values after accumulation
#' @param mask (optional) A binary matrix of size ncol(codes) x ncol(codes). 0s in the mask matrix row i column j indicates that co-occurrence will not be modeled between code i and code j
#' @param window A character, choices are Conversation (or C), MovingStanzaWindow (MSW, MS); default MovingStanzaWindow. Determines how stanzas are constructed, which defines how co-occurrences are modeled
#' @param window.size.back A positive integer, Inf, or character (INF or Infinite), default: 1. Determines, for each line in the data frame, the number of previous lines in a conversation to include in the stanza window, which defines how co-occurrences are modeled
#' @param window.size.forward (optional) A positive integer, Inf, or character (INF or Infinite), default: 0. Determines, for each line in the data frame, the number of subsequent lines in a conversation to include in the stanza window, which defines how co-occurrences are modeled
#' @param ... additional parameters addressed in inner function
#' @param include.meta Locigal indicating if unit metadata should be attached to the resulting ENAdata object, default is TRUE
#' @param groupCol (optional) Column name of data that includes values you wish to group and plot units by. Default is to plot a mean network for all units.
#' @param group1 (optional) Value of groupCol that you wish to group and plot units by
#' @param group2 (optional) Value of groupCol that you wish to group and plot units by
#' @param showPlots Logical indicating if the function should show plots in the Viewer pane immediately after running. Default is TRUE
#'
#'
#'
#' @keywords data, accumulate, set creation, plotting
#'
#' @seealso \code{\link{ENAdata}}, \code{\link{ena.make.set}}, \code{\link{ena.accumulate.data}}, \code{\link{ena.plot.group}}, \code{\link{ena.plot.network}}, \code{\link{ena.plot}}
#'
#' @return \code{\link{ENAset}} class object that can be further processed for analysis or plotting, The \code{\link{ENAplot}}s requested.
#'
#'
##

ena = function(
  data,
  codes,
  units,
  conversation,
  metadata = NULL,
  model = c("EndPoint", "AccumulatedTrajectory", "SeparateTrajectory"),
  weight.by = "binary",
  window = c("MovingStanzaWindow", "Conversation"),
  window.size.back = 1,
  window.size.forward = 0,
  mask = NULL,
  include.meta = TRUE,
  groupCol = NULL,
  group1 = NULL,
  group2 = NULL,
  showPlots = TRUE,
  ...
){
  model = match.arg(model)
  window = match.arg(window)

  accum = ena.accumulate.data(
    units = data[,units],
    conversation = data[,conversation],
    metadata = data[,metadata],
    codes = data[,codes],
    window = window,
    window.size.back = window.size.back,
    window.size.forward = window.size.forward,
    weight.by = weight.by,
    model = model,
    mask = mask,
    include.meta = include.meta

  );

  #Plot mean network of all points if no group column is specified
  if(is.null(groupCol) == TRUE){
    set = ena.make.set(
      enadata = accum
    )

    points = set$points.rotated
    lineweights = set$line.weights
    mean = colMeans(lineweights)

    plot1 = ena.plot(enaset = set, title = "Mean Network -- All Units")
    plot1 = ena.plot.network(plot1, network = mean, colors = "black")
    plot1 = ena.plot.group(plot1, points, colors = "black", labels = "Mean")

    if(showPlots == TRUE){

      print(plot1)
    }

    return(list(set = set, mean.plot = plot1))
  }

  # Plot mean network of one group if user specifies group column and one group
  else if(is.null(group2) == TRUE) {
    if(any(data[,groupCol] == group1) == FALSE){
      stop("Group column does not contain group1 value!")
    }

    set = ena.make.set(
      enadata = accum
    )

    metaNames = data.frame(set$enadata$metadata)
    group1.rows = metaNames[,groupCol] == group1
    group1.points = set$points.rotated[group1.rows,]

    group1.lineweights = set$line.weights[group1.rows,]
    group1.mean = colMeans(group1.lineweights)

    plot1 = ena.plot(set, title = paste0("Mean Network -- ",group1))
    plot1 = ena.plot.network(plot1, network = group1.mean)
    plot1 = ena.plot.group(plot1, group1.points, labels = group1, colors = "red", confidence.interval = "box")

    if(showPlots == TRUE){

      print(plot1)
    }

    return(list(set = set, group1.plot = plot1))
  }

  # Plot mean network subtraction if user specifies group column and two groups
  else {
    if(any(data[,groupCol] == group1) == FALSE){
      stop("Group column does not contain group1 value!")
    }

    if(any(data[,groupCol] == group2) == FALSE){
      stop("Group column does not contain group2 value!")
    }

    set = ena.make.set(
      enadata = accum,
      rotation.by = ena.rotate.by.mean,
      rotation.params = list(accum$metadata[,..groupCol] == group1, accum$metadata[,..groupCol]== group2)
    )

    metaNames = data.frame(set$enadata$metadata)
    group1.rows = metaNames[,groupCol] == group1
    group1.points = set$points.rotated[group1.rows,]

    group2.rows = metaNames[,groupCol] == group2
    group2.points = set$points.rotated[group2.rows,]

    group1.lineweights = set$line.weights[group1.rows,]
    group1.mean = colMeans(group1.lineweights)

    group2.lineweights = set$line.weights[group2.rows,]
    group2.mean = colMeans(group2.lineweights)

    subtracted.network = group1.mean - group2.mean

    #plot group 1 mean network
    plot1 = ena.plot(set, title = paste0("Mean Network -- ",group1))
    plot1 = ena.plot.network(plot1, network = group1.mean)
    plot1 = ena.plot.group(plot1, group1.points, labels = group1, colors = "red", confidence.interval = "box")

    #plot group 2 mean network
    plot2 = ena.plot(set, title = paste0("Mean Network -- ",group2))
    plot2 = ena.plot.network(plot2, network = group2.mean, colors = "blue")
    plot2 = ena.plot.group(plot2, group2.points, labels = group2, colors  = "blue", confidence.interval = "box")

    #Plot subtracted network and means
    plot3 = ena.plot(set, title = paste0("Network Subtraction -- ",group1," vs ",group2))
    plot3 = ena.plot.network(plot3, network = subtracted.network)
    plot3 = ena.plot.group(plot3, group1.points, labels = group1, colors = "red", confidence.interval = "box")
    plot3 = ena.plot.group(plot3, group2.points, labels = group2, colors  = "blue", confidence.interval = "box")

    if(showPlots == TRUE){
      print(plot1)
      print(plot2)
      print(plot3)
    }

    return(list(set = set, group1.plot = plot1, group2.plot = plot2, network.subtraction = plot3))
  }
}
