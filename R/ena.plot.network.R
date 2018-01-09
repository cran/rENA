##
#' @title Plot an ENA network
#'
#' @description Plot an ENA network: nodes and edges
#'
#' @details lots a network graph, including nodes (taken from codes in the ENAplot) and the edges (provided in network)
#'
#' @export
#'
#' @param enaplot \code{\link{ENAplot}} object to use for plotting
#' @param network dataframe or matrix containing the edge weights for the network graph; typically comes from ENAset$line.weights
#' @param node.positions matrix containing the positiions of the nodes. Defaults to enaplot$enaset$node.positions
#' @param colors A String or vector of colors for positive and negative line weights. E.g. red or c(pos= red, neg = blue), default: c(pos= red, neg = blue)
#' @param show.all.nodes A Logical variable, default: true
#' @param threshold A vector of numeric min/max values, default: (0,1). Edge weights below the min value will not be displayed; edge weights above the max value will be shown at the max value.
#' @param thin.lines.in.front A logical, default: true
#' @param opacity A vector of numeric min/max values for opacity, default: (0.3,1)
#' @param saturation A vector of numeric min/max values for saturation, default: (0.25, 1)
#' @param thickness A vector of numeric min/max values for thickness, default: (0, 1)
#' @param node.size A lower and upper bound used for scaling the size of the nodes, default c(0, 20)
#' @param range  A vector of min/max values. Options are numeric, set.min,  set.max, plot.min, plot.max, default: (set.min, set.max). Determines the line weight that corresponds to the thinnest (min) and thickest (max) lines in the network graph
#' @param labels A character vector of node labels, default: code names
#' @param label.offset A numeric vector of an x and y value to offset labels from the coordinates of the points
#' @param label.font.size An integer which determines the font size for graph labels, default: enaplot$font.size
#' @param label.font.color A character which determines the color of label font, default: enaplot$font.color
#' @param label.font.family A character which determines font type, choices: Arial, Courier New, Times New Roman, default: enaplot$font.family
#' @param legend.name A character name to include in the legend. Not included in legend when NULL. Default: NULL
#' @param legend.include.edges Logical value indicating if the edges should be included in the plot
#' @param ... Additional parameters
#'
#' @keywords ENA, plot, network, nodes, edges
#'
#' @seealso \code{\link{ena.plot}}, \code{\link{ena.plot.points}}
#' @importFrom scales rescale

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
#'   enadata = accum,
#'   rotation.by = ena.rotate.by.mean,
#'   rotation.params = list(
#'       accum$metadata$Condition=="FirstGame",
#'       accum$metadata$Condition=="SecondGame"
#'   )
#' )
#'
#' plot = ena.plot(set)
#'
#' unitNames = set$enadata$units
#'
#' ### Subset rotated points and plot Condition 1 Group Mean
#' first.game = unitNames$Condition == "FirstGame"
#' first.game.points = set$points.rotated[first.game,]
#' plot = ena.plot.group(plot, first.game.points, labels = "FirstGame",
#'     colors = "red", confidence.interval = "box")
#'
#' ### Subset rotated points and plot Condition 2 Group Mean
#' second.game = unitNames$Condition == "SecondGame"
#' second.game.points = set$points.rotated[second.game,]
#' plot = ena.plot.group(plot, second.game.points, labels = "SecondGame",
#'     colors  = "blue", confidence.interval = "box")
#'
#' ### get mean network plots
#' first.game.lineweights = set$line.weights[first.game,]
#' first.game.mean = colMeans(first.game.lineweights)
#'
#' second.game.lineweights = set$line.weights[second.game,]
#' second.game.mean = colMeans(second.game.lineweights)
#'
#' subtracted.network = first.game.mean - second.game.mean
#' plot = ena.plot.network(plot, network = subtracted.network)
#' print(plot)
#'
#' @return The \code{\link{ENAplot}} provided to the function, with its plot updated to include the nodes and provided connecting lines.
##
ena.plot.network = function(
  enaplot = NULL,
  network = NULL,
  node.positions = enaplot$enaset$node.positions,
  colors = c(pos="red", "blue"),
  show.all.nodes = T,
  threshold = 0.0,
  thin.lines.in.front = T,
  opacity = c(0.3,1),
  saturation = c(0.25,1),
  thickness = c(0,1),
  node.size = c(3,10),
  range = c(min(network), max(network)),
  labels = rownames(enaplot$enaset$node.positions),
  label.offset = NULL,
  label.font.size = enaplot$get("font.size"),
  label.font.color = enaplot$get("font.color"),
  label.font.family = enaplot$get("font.family"),
  legend.name = NULL,
  legend.include.edges = F,
  ...
) {
  if(choose(nrow(node.positions), 2) != length(network)) {
    stop(paste0("Network vector needs to be of length ", choose(nrow(node.positions), 2)))
  }
  args = list(...);
  network.edges.shapes = list();

  nodes = data.frame(node.positions);
  nodes$weight = rep(0, nrow(nodes))
  nodes$color = "black";
  node.rows = labels; #rownames(enaplot$enaset$node.positions);

  network.scaled = network;
  if(!is.null(args$scale.weights) && args$scale.weights == T) {
    network.scaled = network * (1 / max(abs(network)));
  }

  pos.inds = as.numeric(which(network.scaled >=0));
  neg.inds = as.numeric(which(network.scaled < 0));
  network.opacity = scales::rescale(abs(network.scaled), opacity);
  network.saturation = scales::rescale(abs(network.scaled), saturation);
  network.thickness = scales::rescale(abs(network.scaled), thickness);

  colors.hsv = rgb2hsv(col2rgb(colors))
  if(ncol(colors.hsv) == 1) {
    colors.hsv[[4]] = colors.hsv[1] + 0.5;
    if(colors.hsv[4] > 1) {
      colors.hsv[4] = colors.hsv[4] - 1;
    }

    colors.hsv[[5]] = colors.hsv[2];
    colors.hsv[[6]] = colors.hsv[3];
    dim(colors.hsv) = c(3,2);
  }

  mat = enaplot$enaset$enadata$adjacency.matrix; #attr(enaplot$enaset$enadata$adjacency.vectors.raw,"adjacency.matrix");
  for (i in 1:ncol(mat)) {
    v0 <- node.positions[node.rows==mat[1,i], ];
    v1 <- node.positions[node.rows==mat[2,i], ];
    nodes[node.rows==mat[1,i],]$weight = nodes[node.rows==mat[1,i],]$weight + network.thickness[i];
    nodes[node.rows==mat[2,i],]$weight = nodes[node.rows==mat[2,i],]$weight + network.thickness[i];

    color = NULL
    if(i %in% pos.inds) {
      color = colors.hsv[,1];
    } else {
      color = colors.hsv[,2];
    }
    color[2] = network.saturation[i];

    edge_shape = list(
      type = "line",
      opacity = network.opacity[i],
      nodes = c(mat[,i]),
      line = list(
        name = "test",
        color= hsv(color[1],color[2],color[3]),
        width= network.thickness[i] * enaplot$get("multiplier")
      ),
      x0 = v0[1],
      y0 = v0[2],
      x1 = v1[1],
      y1 = v1[2],
      layer = "below",
      size = as.numeric(abs(network.scaled[i]))
    );
    network.edges.shapes[[i]] = edge_shape
  };

  if(thin.lines.in.front) {
    network.edges.shapes = network.edges.shapes[rev(order(sapply(network.edges.shapes, "[[", "size")))]
  } else {
    network.edges.shapes = network.edges.shapes[order(sapply(network.edges.shapes, "[[", "size"))]
  }
  if(threshold > 0) {
    network.edges.shapes = network.edges.shapes[sapply(network.edges.shapes, "[[", "size") > threshold];
  }
  if(show.all.nodes == F) {
    nodes = nodes[rownames(nodes) %in% unique(as.character(sapply(network.edges.shapes, "[[", "nodes"))), ]
  }
  mode = "markers+text"
  if(!is.null(args$labels.hide) && args$labels.hide == T) {
    mode="markers"
  }
  nodes$weight = scales::rescale((nodes$weight * (1 / max(abs(nodes$weight)))), node.size) # * enaplot$get("multiplier"));
  enaplot$plot = plotly::add_trace(
    enaplot$plot,
    data = nodes,
    x = ~X1,
    y = ~X2,
    mode = mode,
    textposition = 'middle right',
    marker = list(
      color = "#000000",
      size = abs(nodes$weight),
      name = rownames(nodes)[i]
    ),
    text = rownames(nodes),
    legendgroup = legend.name,
    name = legend.name,
    hoverinfo = 'none'
  );

  for(n in 1:length(network.edges.shapes)) {
    e = network.edges.shapes[[n]];

    name = NULL;
    show.legend = F;
    if(!is.null(legend.name) && legend.include.edges) {
      name = paste(e$nodes[1],e$nodes[2], sep=".");
      show.legend = T;
    }
    enaplot$plot = plotly::add_trace(
      enaplot$plot,
      type = "scatter",
      mode = "lines",
      data = data.frame(X1=c(e$x0,e$x1), X2=c(e$y0,e$y1)),
      x = ~X1, y = ~X2,
      line = e$line,
      opacity = e$opacity,
      legendgroup = legend.name,
      showlegend = show.legend,
      name = name
    )
  }

  enaplot
}
