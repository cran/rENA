#' @title ENAplot Class
#' 
#' @description
#' The ENAplot R6 class provides a structure for visualizing ENAset objects using plotly.
#' It encapsulates the ENAset data, the plotly visualization, and related plotting parameters.
#'
#' @section Fields:
#' \describe{
#'   \item{enaset}{The \code{\link{ENAset}} object from which the ENAplot was constructed.}
#'   \item{plot}{The plotly object used for data visualization.}
#'   \item{axes}{Axes information for the plot (TBD).}
#'   \item{point}{Point information for the plot (TBD).}
#'   \item{palette}{Color palette used for plotting (TBD).}
#'   \item{plotted}{Indicates whether the plot has been rendered (TBD).}
#' }
#'
#' @examples
#' # Example usage:
#' # enaplot <- ENAplot$new(enaset = myENAset)
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
#'
#' @field enaset - The \code{\link{ENAset}} object from which the ENAplot was constructed
#' @field plot - The plotly object used for data visualization
#' @field axes A list or object specifying the axes configuration for the ENA plot, such as axis labels, limits, or scaling.
#' @field point A structure representing the data points to be plotted, including coordinates and visual properties.
#' @field palette A set of colors or a function defining the color scheme used for plotting elements in the ENA plot.
#' @field plotted A logical or status indicator showing whether the plot has been rendered or updated.
#' @field showticklabels Logical. Indicates whether to show tick labels on the axes.
#' @field autosize Logical. Indicates whether the plot should automatically resize.
#' @field automargin Logical. Indicates whether the plot should automatically adjust margins.
#' @field axispadding Numeric. Padding factor for the axes.
ENAplot = R6::R6Class("ENAplot",

  public = list(

    ## Public Functions ----
      #' Create ENApolot
      #'
      #' @param enaset An ENA set object containing the data to be plotted.
      #' @param title The title of the plot.
      #' @param dimension.labels Labels for the dimensions shown in the plot.
      #' @param font.size Numeric value specifying the font size for plot text.
      #' @param font.color Color value for the plot text.
      #' @param font.family Font family to use for plot text.
      #' @param scale.to Numeric value to scale the plot axes.
      #' @param showticklabels Logical; whether to display axis tick labels.
      #' @param autosize Logical; whether the plot should automatically size itself.
      #' @param automargin Logical; whether the plot should automatically adjust margins.
      #' @param axispadding Numeric value specifying padding around axes.
      #' @param ... Additional arguments passed to the plotting function.      #' 
      #'
      #' @return ENAplot
      initialize = function(
        enaset = NULL,

        title = "ENA Plot",

        dimension.labels = c("",""),

        font.size = 14,
        font.color = "#000000",
        font.family = "Arial",
        scale.to = "network",
        ...
      ) {
        if (is(enaset, "ENAset")) {
          warning(paste0("Usage of ENAset objects will be deprecated ",
            "and potentially removed altogether in future versions."))

          enaset <- ena.set(enaset);
        }

        code.cols = !colnames(enaset$line.weights) %in% colnames(enaset$meta.data)

        args = list(...);
        if(!is.null(args$multiplier)) {
          private$multiplier = args$multiplier
        }
        if(!is.null(args$point.size)) {
          self$point$size = args$point.size
        }
        if(!is.null(args$showticklabels)) {
          self$showticklabels = args$showticklabels
        }
        if(!is.null(args$axispadding)) {
          self$axispadding = args$axispadding
        }
        if(!is.null(args$autosize)) {
          self$autosize = args$autosize
        }
        if(!is.null(args$automargin)) {
          self$automargin = args$automargin
        }
        self$enaset <- list(
          connection.counts = data.table::copy(enaset$connection.counts),
          meta.data = data.table::copy(enaset$meta.data),
          model = list(
            model.type = enaset$model$model.type,
            raw.input = data.table::copy(enaset$model$raw.input),
            row.connection.counts = data.table::copy(enaset$model$row.connection.counts),
            unit.labels = enaset$model$unit.labels,
            points.for.projection = data.table::copy(enaset$model$points.for.projection),
            centroids = data.table::copy(enaset$model$centroids),
            variance = enaset$model$variance
          ),
          points = data.table::copy(enaset$points),
          line.weights = data.table::copy(enaset$line.weights),
          rotation = list(
            adjacency.key = data.table::copy(enaset$rotation$adjacency.key),
            codes = enaset$rotation$codes,
            rotation.matrix = data.table::copy(enaset$rotation$rotation.matrix),
            center.vec = enaset$rotation$center.vec,
            nodes = data.table::copy(enaset$rotation$nodes)
          ),
          plots = list()
        );
        self$title <- title;

        private$dimension.labels <- dimension.labels;
        private$font.size <- font.size;
        private$font.color <- font.color;
        private$font.family <- font.family;
        private$font = list (
          size = private$font.size,
          color = private$font.color,
          family = private$font.family
        );
        self$plot <- plotly::plot_ly(
          mode = "markers",
          type ="scatter"
        );

        self$plot <- plotly::config(p = self$plot, displayModeBar = args$displayModeBar);

        if (is.list(scale.to)) {
          max.axis = max(abs(as.matrix(enaset$points)))*self$axispadding
          if(is.null(scale.to$x)) {
            axis.range.x = c(-max.axis, max.axis)
          }
          else {
            axis.range.x = scale.to$x
          }
          if(is.null(scale.to$y)) {
            axis.range.y = c(-max.axis, max.axis)
          }
          else {
            axis.range.y = scale.to$y
          }
        }
        else {
          if(is.character(scale.to) && scale.to == "points") {
            max.axis = max(abs(as.matrix(enaset$points)))*self$axispadding
          }
          else if (is.numeric(scale.to)) {
            max.axis = tail(scale.to, 1)
          }
          else {
            max.axis = max(abs(as.matrix(enaset$rotation$nodes)))*self$axispadding;
          }
          axis.range.x = axis.range.y = c(-max.axis, max.axis)
        }

        graph.axis <- list(
          titlefont = private$font,
          showgrid = F,
          zeroline = T,
          showticklabels = self$showticklabels,
          showgrid = T
          # range=c(-max.axis,max.axis)
        );
        if(!is.null(args$ticks)) {
          graph.axis$showticklabels = T;
          graph.axis$ticks = args$ticks$location;
          graph.axis$tickcolor = args$ticks$color;
          graph.axis$tickangle = args$ticks$angle;
        }
        self$axes$x = graph.axis
        self$axes$x$title = dimension.labels[1];
        self$axes$x$range = axis.range.x
        self$axes$y = graph.axis
        self$axes$y$title = dimension.labels[2];
        self$axes$y$range = axis.range.y

        self$plot = plotly::layout(
          self$plot,
          title =  title,
          xaxis = self$axes$x,
          yaxis = self$axes$y,
          autosize = self$autosize,
          font = list (
            size = 12,
            color = private$font.color,
            family = private$font.family
          )
        );
      },

      #' Print ENA plot
      #'
      #' @return
      print = function() {
        print(self$plot);
      },

      #' Get property from object
      #'
      #' @param x character key to retrieve from object
      #' @return value from object at x
      get = function(x) {
        return(private[[x]])
      },

    ## Public Properties ----
      enaset = NULL,
      title = "ENA Plot",
      plot = NULL,
      axes = list(
        x = NULL, y = NULL
      ),
      point = list(
        size = 5
      ),
      showticklabels = F,
      autosize = F,
      automargin = T,
      axispadding = 1.2,
      palette = c("#386CB0", "#F0027F", "#7FC97F", "#BEAED4",
                  "#FDC086","#FFFF99", "#BF5B17"),
      plotted = list(
        points = list(), networks = list(),
        trajectories = list(), means = list()
      )
  ),

  private = list(
    ####
    ## Private Properties
    ####
      dimension.labels = c("X","Y"),

      font = list(),
      font.size = 14,
      font.color = "#000000",
      font.family = "Arial",
      #plot.color = I("black"),

      multiplier = 5
    ####
    ## END: Private Properties
    ####
  )
)
