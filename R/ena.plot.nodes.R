
ena.plot.nodes = function(

  enaplot = NULL,

  nodes = enaplot$enaset$node.positions,
  labels = rownames(enaplot$enaset$codes), ### if supplied - must be vector of equal size as nodes

  label.font.size = enaplot$get("font.size"),
  label.font.color = enaplot$get("font.color"),
  label.font.family = enaplot$get("font.family"),

  node.size = 6

) {
  # df = data.frame(enaplot$enaset$line.weights, attr(enaplot$enaset$line.weights, opts$UNIT_NAMES));
  # dfDT= data.table::as.data.table(df);
  #
  # dfDT$handle = merge_columns_c(dfDT, units.by, sep="."); #rownames(df);
  #
  # units.to.plot = c(selection.one.name, selection.two.name);
  #
  # sdcols=colnames(dfDT)[sapply(dfDT, is.numeric)];
  #
  # minDT = dfDT[handle %in% units.to.plot, lapply(.SD,sum,na.rm=T), by=units.by, .SDcols=sdcols];
  #
  # minDT$ENA_UNIT = merge.columns(x = minDT, from.cols = units.by);
  # minDT = minDT[match(ENA_UNIT, units.to.plot),];
  #
  # minDTc = minDT[,apply(.SD,2,make.network.node, types=c(selection.one.color, selection.two.color)),.SDcols=sdcols, with = T];
  # minDTsizes = minDTc[1,!is.na(minDTc[2,]), with=F];
  # minDTcolors = minDTc[2,!is.na(minDTc[2,]), with=F];
  # minDTsizes = minDTsizes[,which(!names(minDTcolors) %in% group.by),with=F];
  # minDTcolors = minDTcolors[,which(!names(minDTcolors) %in% group.by),with=F];
  # minDTnodes = minDTc[,!is.na(minDTc[2,]), with=F]
  # minDTnodes = minDTnodes[,which(!names(minDTcolors) %in% group.by),with=F]
  # minDTnodes = minDTnodes[,{ cols=strsplit(colnames(.SD), "...", fixed=T); m=as.matrix(.SD[,,with=F]); lapply(1:length(cols),function(x){ c(m[1,x],m[2,x],cols[[x]]) }); },];
  #
  # minDTnodes_trans = t(minDTnodes);
  # minDTnodes_trans = minDTnodes_trans[,c(3:4,1:2)];
  #
  # network.edges = minDTnodes_trans; #as.data.frame(get.edgelist(network.graph));
  # network.edges.table = data.table::as.data.table(network.edges);
  #
  # ## Remove edges below threshold
  # network.edges.table = network.edges.table[V3 > network.edge.threshold]
  #
  #
  #
  # df.names = rownames(enaplot$enaset$node.positions);
  # if(is.null(df.names)) {
  #   df.names = as.character(1:nrow(enaplot$enaset$node.positions))
  #   rownames(enaplot$enaset$node.positions) = df.names;
  # }
  # network.vertices.df = data.frame(
  #   name = df.names, ## New LWS method needs to assign names/attr
  #   enaplot$enaset$node.positions
  # );
  # network.graph = igraph::graph_from_data_frame(
  #   minDTnodes_trans,
  #   directed = F,
  #   vertices = network.vertices.df
  # )
  # network.layout = enaplot$enaset$node.positions;
  # network.vertices = igraph::V(network.graph);
  # network.vertices.length = length(network.vertices);
  # network.font.text = list(
  #   family = node.font.family,
  #   size = node.font.size,
  #   color = node.font.color
  # )
  #
  # network.nodes.x = network.layout[,1];
  # network.nodes.y = network.layout[,2];
  #
  # network.edges.shapes = list();
  # for (i in 1:network.edges.length) {
  #   # browser()
  #   v0 <- unlist(network.edges.table[i,][[1]]); #network.edges.table[i,][[1]];
  #   v1 <- unlist(network.edges.table[i,][[2]]); #network.edges.table[i,][[2]];
  #   edge_shape = list(
  #     type = "line",
  #     line = list(
  #       color=unlist(network.edges.table[i,][[4]]), #network.edges.table[i,][[4]],
  #       width=unlist(network.edges.table[i,][[3]]) * edge.weight.multiplier #network.edges.table[i,][[3]]
  #     ),
  #     x0 = network.vertices.df[v0,]$X1, #network.nodes.x[0],
  #     y0 = network.vertices.df[v0,]$X2, #network.nodes.y[0],
  #     x1 = network.vertices.df[v1,]$X1, #network.nodes.x[1],
  #     y1 = network.vertices.df[v1,]$X2 #network.nodes.y[1]
  #   );
  #   network.edges.shapes[[i]] = edge_shape
  # }
  #
  # network.graph.axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = T);
  #
  # #### SIZES DETERMINED BY edge weights - how to handle when only plotting nodes????
  # node.sizes = sapply(rownames(network.layout), function(x) { network.edges.table[V1==x|V2==x, sum(unlist(V3)),] }) * node.weight.multiplier
  #
  # if(!network.show.all.codes) {
  #   node.sizes = node.sizes[which(data.frame(node.sizes)$node.sizes != 0)]
  #   network.layout = network.layout[rownames(network.layout) %in% names(node.sizes),]
  # }

  textstyle = list(
    family = label.font.family,
    size = label.font.size,
    color = label.font.color
  );

  enaplot$plot = plotly::add_data(enaplot$plot, data.frame(nodes))
  enaplot$plot = plotly::add_markers(
    enaplot$plot,
    data = data.frame(nodes),
    x = ~X1,
    y = ~X2,
    mode = "markers",
    marker = list(
      color = I("black"),
      size = node.size
    ),
    showlegend = F,
    text = rownames(nodes),
    #textfont = textstyle,   #### can only use for label text not hovertext
    hoverinfo = "text+x+y"
  );
  # enaplot$plot %<>% plotly::layout(
  #   text = rownames(nodes),
  #   textfont = textstyle,
  #   hoverlabel = "text+x+y"
  # );
  ### ADD ANNOTATIONS
  # enaplot$plot %<>% plotly::add_annotations(
  #   text = rownames(network.layout),
  #   textfont = network.font.text,
  #   xref = "x",
  #   yref = "y",
  #   xanchor = "center",
  #   standoff = 30,
  #   #clicktoshow = "onout",
  #   #captureevents = T,
  #   visible = F,
  #   #ax = 20, #sample(200, nrow(network.layout), replace=T),
  #   #ay = -90,
  #   showarrow = F
  #   #textposition = "top right"
  # );


  # enaplot$plot %<>% plotly::layout(
  #   title =  stringr::str_c(selection.one.title, selection.two.title, sep = " - "),
  #   shapes = network.edges.shapes,
  #   xaxis = network.graph.axis,
  #   yaxis = network.graph.axis
  # )

  enaplot;
}
