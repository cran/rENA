##
#' @title Plot of ENA set groups
#'
#' @description Plot a point based on a summary statistic computed from a given method (typically, mean) for a set of points in a projected ENA space
#'
#' @details Plots a point based on a summary statistic for a group (typically, mean)
#'
#' @export
#'
#' @param enaplot \code{\link{ENAplot}} object to use for plotting
#' @param points A matrix or data.frame where columns contain coordinates of points in a projected ENA space
#' @param method A function for computing a summary statistic for each column of points
#' @param labels A character which will be the label for the group's point
#' @param colors A character, determines color of the group's point, default: enaplot$color
#' @param shape A character, determines shape of the group's point, choices:  square, triangle, diamond, circle, default: square
#' @param confidence.interval A character that determines how the confidence interval is displayed, choices: none, box, crosshair, default: none
#' @param outlier.interval A character that determines how outlier interval is displayed, choices: none, box, crosshair, default: none
#' @param label.offset A numeric vector containing an x and y value to offset label for the group's point from the coordinates of the point
#' @param label.font.size An integer which determines the font size for label, default: enaplot\$font.size
#' @param label.font.color A character which determines the color of label, default: enaplot\$font.color
#' @param label.font.family A character which determines font type, choices: Arial, Courier New, Times New Roman, default: enaplot\$font.family
#' @param show.legend Logical indicating whether to show the point labels in the in legend
#' @param ... Additional parameters
#'
#' @import magrittr
#' @keywords ENA, plot, group
#'
#' @seealso \code{\link{ena.plot}}, \code{ena.plot.points}
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
#' print(plot);
#'
#' @return The  \code{\link{ENAplot}} provided to the function, with its plot updated to include the new group point.
##
ena.plot.group <- function(
  enaplot,
  points = NULL,
  method = "mean",
  labels = NULL,
  colors = "black",
  shape = c("square", "triangle-up", "diamond", "circle"),
  confidence.interval = c("none", "crosshairs", "box"),
  outlier.interval = c("none", "crosshairs", "box"),
  label.offset = NULL,
  label.font.size = NULL,
  label.font.color = NULL,
  label.font.family = NULL,
  show.legend = T,
  ...
) {
  shape = match.arg(shape);
  confidence.interval = match.arg(confidence.interval);
  outlier.interval = match.arg(outlier.interval);

  if(is.null(points)) {
    stop("Points must be provided.");
  }

  ### problem if outlier and confidence intervals selected for crosshair
  if(confidence.interval == "crosshair" && outlier.interval == "crosshair") {
    print("Confidence Interval and Outlier Interval cannot both be crosshair");
    print("Plotting Outlier Interval as box");
    outlier.interval = "box";
  }

  ### if group more than one row, combine to mean
  confidence.interval.values = NULL;
  outlier.interval.values = NULL;
  if(
    (is(points, "data.frame") || is(points, "matrix")) &&
    nrow(points) > 1
  ){
    if(confidence.interval != "none") {
      confidence.interval.values = t.test(points, conf.level = .95)$conf.int;
    }
    if(outlier.interval != "none") {
      outlier.interval.values = c(IQR(points[,2]), IQR(points[,2])) * 1.5;
    }

    if(is.null(method) || method == "mean") {
      points = colMeans(points);
    } else {
      points = do.call(method, args = list(points))
    }
  }

  enaplot %<>% ena.plot.points(
    points = points,
    labels = labels,
    colors = colors,
    shape = shape,
    confidence.interval = confidence.interval,
    confidence.interval.values = confidence.interval.values,
    outlier.interval = outlier.interval,
    outlier.interval.values = outlier.interval.values,
    label.offset = label.offset,
    label.font.size = label.font.size,
    label.font.color = label.font.color,
    label.font.family = label.font.family,
    show.legend = show.legend,
    ...
  )
  return(enaplot)

  #
  # group.layout = data.frame(dfDT.points);
  #
  # ### INTERVAL CALCULATIONS
  # error = NULL;
  # lines = list();
  #
  # if(confidence.interval == "crosshair") {
  #   ci.x = t.test(points.raw, conf.level = .95)$conf.int[1];
  #   ci.y = t.test(points.raw, conf.level = .95)$conf.int[2];
  #   error = list(
  #     x = list(type = "data", array = ci.x),
  #     y = list(type = "data", array = ci.y)
  #   )
  # } else if(outlier.interval == "crosshair") {
  #   oi.x = IQR(points.raw$V1) * 1.5;
  #   oi.y = IQR(points.raw$V2) * 1.5;
  #   error = list(
  #     x = list(type = "data", array = oi.x),
  #     y = list(type = "data", array = oi.y)
  #   )
  # }
  #
  # if(confidence.interval == "box") {
  #
  #   conf.ints = t.test(points.raw, conf.level = .95)$conf.int;
  #   dfDT.points[,c("ci.x", "ci.y") := .(conf.ints[1], conf.ints[2])]
  #
  #   #add cols for coordinates of CI lines
  #   dfDT.points[, c("ci.x1", "ci.x2", "ci.y1", "ci.y2") := .(V1 - ci.x, V1 + ci.x, V2 - ci.y, V2 + ci.y)]
  #
  #   lines.CI = apply(dfDT.points,1,function(x) {
  #     list(
  #       "type" = "square",
  #       "line" = list(
  #         width = 1,
  #         color = color,
  #         dash="dash"
  #       ),
  #       "xref" = "x",
  #       "yref" = "y",
  #       "x0" = x[['ci.x1']],
  #       "x1" = x[['ci.x2']],
  #       "y0" = x[['ci.y1']],
  #       "y1" = x[['ci.y2']]
  #     );
  #   });
  #   lines = lines.CI;
  # }
  # if(outlier.interval == "box") {
  #
  #   oi.x = IQR(points.raw$V1) * 1.5;
  #   oi.y = IQR(points.raw$V2) * 1.5;
  #
  #   dfDT.points[,c("oi.x", "oi.y") := .(oi.x, oi.y)]
  #
  #   #add cols for coordinates of CI lines
  #   dfDT.points[, c("oi.x1", "oi.x2", "oi.y1", "oi.y2") := .(V1 - oi.x, V1 + oi.x, V2 - oi.y, V2 + oi.y)]
  #
  #   lines.OI = apply(dfDT.points,1,function(x) {
  #     list(
  #       "type" = "square",
  #       "line" = list(
  #         width = 1,
  #         color = color,
  #         dash="dash"
  #       ),
  #       "xref" = "x",
  #       "yref" = "y",
  #       "x0" = x[['oi.x1']],
  #       "x1" = x[['oi.x2']],
  #       "y0" = x[['oi.y1']],
  #       "y1" = x[['oi.y2']]
  #     );
  #   });
  #
  #   lines = c(lines, lines.OI);
  # }
  #
  #
  # if(!is.null(error)) {
  #   #plot group w/ crosshair error bars
  #   enaplot$plot = plotly::add_trace(
  #     enaplot$plot,
  #     data = group.layout,
  #     type="scatter",
  #     x = ~V1, y = ~V2,
  #     mode="markers",
  #     marker = list(
  #       symbol =  shape,
  #       color = color,
  #       size = size
  #     ),
  #     error_x = error$x,
  #     error_y = error$y,
  #     showlegend = F,
  #     text = label,
  #     hoverinfo = "text+x+y"
  #   )
  # } else {
  #   #plot group w/o crosshair error bars
  #   enaplot$plot = plotly::add_trace(
  #     enaplot$plot,
  #     data = group.layout,
  #     type="scatter",
  #     x = ~V1, y = ~V2,
  #     mode="markers",
  #     marker = list(
  #       symbol =  shape,  #c(rep("circle",nrow(data)),rep("square", ifelse(!is.null(dfDT.groups), nrow(dfDT.groups), 0))),
  #       color = color,
  #       #size = c(rep(unit.size * unit.size.multiplier, nrow(data)), rep(group.size, ifelse(!is.null(dfDT.groups),nrow(dfDT.groups), 0)))
  #       size = size
  #     ),
  #     showlegend = F,
  #     text = label,
  #     hoverinfo = "text+x+y"
  #   )
  # }
  #
  # ##### WEIGHTING OFFSET
  # if(is.null(label.offset)) { label.offset = c(.05,.05) }
  # else label.offset = c(label.offset[1] * 0.1, label.offset[2] * 0.1)
  #
  # enaplot$plot = plotly::add_annotations(
  #   enaplot$plot,
  #   x = group.layout$V1[1] + label.offset[1],
  #   y = group.layout$V2[1] + label.offset[2],
  #   text = label,
  #   font = text.info,
  #   xref = "x",
  #   yref = "y",
  #   ax = label.offset[1],
  #   ay = label.offset[2],
  #   #xanchor = "left",
  #   showarrow = F
  # );
  #
  # enaplot$plot = plotly::layout(
  #   enaplot$plot,
  #   shapes = lines
  #   #annotations = label.info
  # )
  #
  # return(enaplot);
}
