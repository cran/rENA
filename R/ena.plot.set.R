##
# @title Generate Plot of ENA set
#
# @description Generate a plot of a given ENA set.
#
# @details [TBD]
#
#
# @param enaset \code{\link{ENAset}} used to generate the plot
# @param plot.title Title of the plot
# @param plot.mode [TBD]
# @param plot.color Color of the plot
# @param dimensions Number of dimensions in the plot
# @param dimension.labels Labels for the dimensions of the plot
# @param dimension.show.variance TBD
# @param multiplier Size multiplier for the plot
# @param units Vector of units to plot
# @param unit.colors Color of units
# @param unit.labels Names of units
# @param unit.labels.positions Location of names of units
# @param unit.size change size of unit label
# @param unit.size.multiplier Change size multiplier of unit labels
# @param unit.show.confidence.intervals Show confidence intervals of a unit
# @param unit.group [TBD]
# @param unit.group_by [TBD]
# @param unit.group.labels [TBD]
# @param unit.group.labels.positions [TBD]
# @param unit.group.labels.colors [TBD]
# @param unit.group.size [TBD]
# @param unit.group.size.multiplier [TBD]
# @param unit.trajectory.by [TBD]
# @param network.one [TBD]
# @param network.two [TBD]
# @param network.colors [TBD]
# @param network.show.all.codes [TBD]
# @param network.code.labels [TBD]
# @param network.code.labels.positions [TBD]
# @param network.edge.threshold [TBD]
# @param axis.flip.x Flip plot on x-axis
# @param axis.flip.y Flip plot on y-axis
# @param estimate.network.over Adjusts weights using what is being plotted or what is in the entire set
# @param ... Additional parameters addressed in inner function
#
# @keywords ENA, plot, set
#
# @seealso \code{\link{ena.make.set}}, \code{ena.update.set}
#
# @examples
# \dontrun{
# # Given an ENA set
# ena.plot.set(\code{\link{ENAset}})
#
# }
# @return Plot of \code{\link{ENAset}}
##
ena.plot.set <- function(
  enaset,
  plot.title = "ENA Plot",
  plot.mode = "units+network",
  plot.color = I("black"),

  dimensions = c(1,2),
  dimension.labels = c("x","y"),
  dimension.show.variance = T,

  multiplier = 5,

  units = unique(enaset$enadata$get("units")),
  unit.colors = rep(plot.color, length(units)),
  unit.labels = units,
  unit.labels.positions = NULL,
  unit.size = 1,
  unit.size.multiplier = multiplier,
  unit.show.confidence.intervals = T,

  unit.group = NULL,
  unit.group_by = c("mean","sum"),
  unit.group.labels = names(unit.groups),
  unit.group.labels.positions = "top right",
  unit.group.labels.colors = rep(plot.color, length(unit.group)),
  unit.group.size = unit.size,
  unit.group.size.multiplier = unit.size.multiplier,

  unit.trajectory.by = NULL,

  network.one = NULL,
  network.two = NULL,
  network.colors = NULL,
  network.show.all.codes = F,
  network.code.labels = rownames(enaset$nodes$positions$scaled),
  network.code.labels.positions = NULL,
  network.edge.threshold = 0,

  axis.flip.x = F,
  axis.flip.y = F,

  estimate.network.over = c("plot","set"),

  ...
) {
  plot = NULL

  show.modes = unlist(strsplit(plot.mode,split="\\+"))
  show.units = "units" %in% show.modes;
  show.networks = "network" %in% show.modes;

  unit.group_by <- match.arg(unit.group_by);
  estimate.network.over <- match.arg(estimate.network.over);

  if(show.units && !is.null(units)){
    plot = ena.plot.units(
      enaset = enaset, plot.title = plot.title,

      units = units,
      unit.size = unit.size,
      unit.colors = unit.colors,
      unit.group = unit.group,
      unit.show.confidence.intervals = unit.show.confidence.intervals,
      unit.group.size = unit.group.size,
      unit.group.size.multiplier = unit.group.size.multiplier,
      unit.trajectory.by = unit.trajectory.by,

      dimension.labels = dimension.labels,
      dimension.show.variance = dimension.show.variance
    );
  }
  if(show.networks && !is.null(network.one)) {
    plot = ena.plot.network(
      enaset = enaset,
      plot = plot,
      selection.one.name = network.one,
      selection.two.name = network.two,

      network.edge.threshold = network.edge.threshold,
      network.show.all.codes = network.show.all.codes
    )
  }
  return(plot);
}
