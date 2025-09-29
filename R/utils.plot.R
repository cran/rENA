#' Plot an ena.set object
#'
#' @param x ena.set to plot
#' @param y ignored.
#' @param ... Additional parameters passed along to ena.plot functions
#' @param empty Logical; if TRUE, creates an empty plot without points. Default is TRUE.
#' @param title Character; title for the plot. Default is "ENA Plot".
#'
#' @examples
#'
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
#' plot(set) |>
#'   add_points(Condition$FirstGame, colors = "blue", with.mean = TRUE) |>
#'   add_points(Condition$SecondGame, colors = "red", with.mean = TRUE) |>
#'   with_means() |>
#'   add_nodes()
#'
#' myENAplot <- plot(set) |>
#'   add_network(Condition$FirstGame - Condition$SecondGame)
#' 
#' 
#' # Add a group mean to an existing ENA plot
#' add_group(myENAplot, wh = Condition$FirstGame)
#' 
#' # Add a trajectory to an existing ENA plot
#' add_trajectory(myENAplot, wh = Condition$FirstGame)
#' 
#' @example inst/examples/example-plot-piping.R
#' 
#' @return ena.plot.object
#' @export
plot.ena.set <- function(x, y, ..., empty = TRUE, title = "ENA Plot") {
  args <- list(...);

  if(is(x, "ena.ordered.set")) {
    stop("Plotting of ena.ordered.set objects requires using the 'ona' package.");
  }

  p = ena.plot(enaset = x, title = title, ...);
  if (isFALSE(empty)) {
    add_points(p, ...);
  }

  return(p)
}


#' Add points to an ENA plot
#'
#' This function adds points to an existing ENA plot or ENA set. It supports various input types for the `wh` parameter, including unevaluated expressions and language objects.
#'
#' @param x An `ENAplot` object or an ENA set containing plots.
#' @param wh Specifies the points to plot. Can be an unevaluated expression or a language object.
#' @param ... Additional parameters passed to the plotting functions.
#' @param colors A vector of colors for the plotted points. Default is `NULL`.
#'
#' @details
#' The function determines the type of the `wh` parameter and processes it accordingly:
#' - If `wh` is an unevaluated expression, it is captured and evaluated in the parent frame.
#' - If `wh` is a language object, it is processed to extract the relevant points information.
#'
#' The function updates the plot with the new points and stores the updated plot back in the ENA set.
#'
#' @example inst/examples/example-plot-piping.R
#' 
#' @return Invisibly returns the modified ENA set.
#'
#' @export
add_points <- function(
  x,
  wh = NULL, ...,
  colors = NULL
) {
  plot <- x;
  set <- plot$enaset;

  if(is.null(plot)) {
    stop("No existing plot found in the ENA set. Did you call plot(set) first?")
  }
  # plot <- set$plots[[length(set$plots)]]
  more.args <- list(...)

  wh_subbed <- substitute(wh)
  if (is.language(wh_subbed)) {
    # points <- list(do.call(`[`, list(x = set$points, i = wh)));
    points <- list(eval(str2lang(paste0(c("set$points", wh_subbed), collapse = "$"))));
    colors <- ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors);
    named <- paste(as.character(wh_subbed)[-1], collapse = " ");
  }
  else if (!is.null(wh_subbed) && length(wh_subbed) > 0) {
    wh_subbed <- as.character(wh_subbed);
    if (length(wh_subbed) > 1 && wh_subbed[[2]] %in% colnames(set$points)) {
      cc <- call(wh_subbed[[1]], set$points, wh_subbed[[2]])
      part1 <- eval(cc);
      name <- paste(wh_subbed[-1], collapse = "$");
      if(grepl(set$model$model.type, pattern="Trajectory")) {
        points <- set$points[part1 == wh_subbed[[3]], ]
        more.args$points = points[, .SD[nrow(.SD)], by = ENA_UNIT]
      }
      else {
        more.args$points = points <- set$points[part1 == wh_subbed[[3]], ]
      }

      if(is.null(colors)) {
        colors = plot$palette[length(plot$plotted$points) + 1]
      }
    }
    else if (length(wh_subbed) == 1 && wh_subbed[[1]] %in% colnames(set$points)) {
      more.args$points = points = set$points
      if(is.null(colors)) {
        colors <- plot$palette[as.numeric(as.factor(set$points[[wh_subbed]])) + length(plot$plotted$points)]
      }
      else {
        colors <- colors[as.numeric(as.factor(set$points[[wh_subbed]]))]
      }
    }
    else {
      points <- wh
      colors = ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors)
    }
  }
  else {
    # first_meta <- setdiff(colnames(set$connection.counts)[find_meta_cols(set$connection.counts)], c("QEUNIT", "ENA_UNIT"))[1]
    # meta_grps <- split(set$points, by = first_meta)
    
    # points = meta_grps
    # named <- paste0(names(points), ".Points")
    # # colors = ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors)
    # colors <- plot$palette[seq.int(from=length(plot$plotted$points)+1,length.out=length(meta_grps))];
    points <- list(set$points);
    named <- "all.points";
    colors <- ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors);
  }

  mean <- ifelse(!is.null(more.args$mean), more.args$mean, FALSE);
  more.args$enaplot = plot
  for(i in seq_along(colors)) {
    color <- colors[i];
    name <- named[i];
    pts <- points[[i]];
    more.args$colors <- color;
    more.args$legend.name <- name;
    more.args$points <- pts;
    plot <- do.call(ena.plot.points, more.args);

    plot$plotted$points[[length(plot$plotted$points) + 1]] <- list(
      data = points,
      color = color
    )
    names(plot$plotted$points)[length(plot$plotted$points)] <- name;

    if(isTRUE(mean) && nrow(pts) > 1) {
      more.args$labels <- name;
      plot <- do.call(ena.plot.group, more.args);
    }
  }
  # if(!is.null(colors)) {
  #   more.args$colors = colors
  # }
  # else {
  #   more.args$colors = plot$palette[length(plot$plotted$points) + 1]
  # }
  

  # if(!is.null(mean) && (is.list(mean) || mean == T)) {
  #   # if (is.list(mean)) {
  #   #   more.args <- c(mean, more.args[!names(more.args) %in% names(mean)])
  #   # }
  #   # more.args$enaplot <- plot
  #   # more.args$points <- points
  #   # more.args$labels <- name
  #   #
  #   # plot <- do.call(ena.plot.group, more.args).
  #   set <- add_group(set, substitute(wh), ...);
  # }

  # set$plots[[length(set$plots)]] <- plot

  return(plot);
}

#' Add all groups to an ENA plot
#'
#' This function iterates over all unique values of the first metadata column (excluding 'QEUNIT' and 'ENA_UNIT')
#' in the ENA set and adds each group as a set of points to the ENA plot. This is useful for quickly visualizing
#' all groups in a categorical variable on the same plot.
#'
#' @param x An `ENAplot` object (as returned by `plot.ena.set`).
#' @param wh (Ignored) Included for compatibility with other plotting functions.
#'
#' @details
#' The function finds the first metadata column in the ENA set (excluding 'QEUNIT' and 'ENA_UNIT'),
#' and for each unique value in that column, calls `add_points()` to add the group's points to the plot.
#'
#' @return The modified `ENAplot` object with all groups added as points.
#' 
#' @example inst/examples/example-plot-piping.R
#'
#' @export
group <- function(x, wh = NULL) {
  plot <- x;
  set <- plot$enaset;

  first_meta <- setdiff(colnames(set$connection.counts)[find_meta_cols(set$connection.counts)], c("QEUNIT", "ENA_UNIT"))[1]
  
  plot$plotted$points <- list();
  # meta_grps <- split(set$points, by = first_meta);
  meta_grps <- unique(set$points[[first_meta]]);
  for(grp in meta_grps) {
    add_points(plot, wh = call("==", as.name(first_meta), grp));
  }

  # points = meta_grps
  return(plot);
}

#' Add a trajectory to an ENA plot
#'
#' This function adds a trajectory to an existing ENA plot or ENA set. It supports various input types for the `wh` parameter, including unevaluated expressions and language objects.
#'
#' @param x An `ENAplot` object or an ENA set containing plots.
#' @param wh Specifies the trajectory to plot. Can be an unevaluated expression or a language object.
#' @param ... Additional parameters passed to the plotting functions.
#' @param name A character string specifying the name of the plot. Default is "plot".
#'
#' @details
#' The function determines the type of the `wh` parameter and processes it accordingly:
#' - If `wh` is an unevaluated expression, it is captured and evaluated in the parent frame.
#' - If `wh` is a language object, it is processed to extract the relevant trajectory information.
#'
#' The function updates the plot with the new trajectory and stores the updated plot back in the ENA set.
#'
#' @return Invisibly returns the modified ENA set.
#'
#' @example inst/examples/example-plot-piping.R 
#'
#' @export
add_trajectory <- function(x, wh = NULL, ..., name = "plot") {
  plot <- x;
  set <- plot$enaset;

  subbed <- substitute(wh)
  args_list <- as.character(subbed)
  points <- set$points

  if (!is.null(args_list) && !is.null(subbed)) {
    if (length(args_list) > 1) {
      wh_subbed <- as.character(substitute(wh))
      cc <- call(wh_subbed[[1]], set$points, wh_subbed[[2]])
      part1 <- eval(cc)
      points <- set$points[part1 == wh_subbed[[3]], ]
      by <- "ENA_UNIT"
    }
    else {
      by <- args_list[[1]]
    }
  }
  else {
    by <- "ENA_UNIT"
  }
  plot <- ena.plot.trajectory(plot, points = points, by = by)

  # set$model$plot <- plot
  # set$plots[[length(x$plots)]] <- plot
  
  # .return(set, from_plot = T, invisible = F)
  return(plot)
}


#' Add a group mean to an ENA plot
#'
#' This function adds a group mean to an existing ENA plot or ENA set. It supports various input types for the `wh` parameter, including unevaluated expressions and language objects.
#'
#' @param x An `ENAplot` object or an ENA set containing plots.
#' @param wh Specifies the group to plot. Can be an unevaluated expression or a language object.
#' @param ... Additional parameters passed to the plotting functions.
#'
#' @details
#' The function determines the type of the `wh` parameter and processes it accordingly:
#' - If `wh` is an unevaluated expression, it is captured and evaluated in the parent frame.
#' - If `wh` is a language object, it is processed to extract the relevant group information.
#'
#' The function updates the plot with the new group mean and stores the updated plot back in the ENA set.
#'
#' @example inst/examples/example-plot-piping.R
#' 
#' @return Invisibly returns the modified ENA set.
#'
#' @export
add_group <- function(x, wh = NULL, ...) {
  plot <- x;
  set <- plot$enaset;

  # Capture the expression passed to wh
  wh.expr <- substitute(wh)

  # Check if the expression is a call to `substitute()`. This happens when
  # add_group is called from another function like add_points, which has
  # already substituted the user's original input.
  if (is.call(wh.expr) && deparse(wh.expr[[1]]) == "substitute") {
    # If so, evaluate it in the parent frame to get the actual language object
    wh.clean <- eval(wh.expr, parent.frame())
  } else {
    # Otherwise, the captured expression is what we want
    wh.clean <- wh.expr
  }

  # set <- x
  # # plot <- set$model$plot
  # # plot <- set$plots[[length(set$plots)]]

  if (
    identical(as.character(wh.clean), "wh.clean") ||
    identical(as.character(wh.clean), "y")
  ) {
    wh.clean <- wh;
  }

  more_args = list(...)
  more_args$enaplot <- plot
  if(is.null(more_args$color)) {
    more_args$colors <- plot$palette[length(plot$plotted$means) + 1]
  }
  else {
    more_args$colors <- more_args$color;
  }

  group.rows.log <- NULL;
  if (is.null(wh.clean)) {
    plot <- do.call(ena.plot.group, more_args)
    group.rows.log <- rep(TRUE, nrow(set$points));
  }
  else {
    parts <- as.character(wh.clean)

    if (parts[2] %in% colnames(set$line.weights)) {
      label <- parts[3]
      group.rows.log <- set$points[[parts[2]]] == parts[3];
      group.rows <- set$points[group.rows.log, ]
      if(nrow(group.rows) > 0) {
        group.means <- colMeans(group.rows)

        more_args$points <- group.means
        more_args$labels <- label
        plot <- do.call(ena.plot.group, more_args)
      }
      else {
        warning("No points in the group")
      }
    }
    else {
      warning("Unable to plot group")
    }
  }

  plot$plotted$means[[length(plot$plotted$means) + 1]] = list(
    rows = group.rows.log,
    data = more_args$points,
    color = more_args$colors
  )

  # set$plots[[length(set$plots)]] <- plot
  
  # .return(plot, from_plot = T, invisible = F)
  return(plot)
}


##' Add a network to an ENA plot
#'
#' Adds a network (set of edges) to an existing ENA plot or ENA set. The network can be specified in several ways, including as an unevaluated expression, a numeric matrix, or a language object. This function is typically used to visualize group means, differences between groups, or custom networks on an ENA plot.
#'
#' @param x An `ENAplot` object or an ENA set containing plots.
#' @param wh Specifies the network to plot. Can be:
#'   \itemize{
#'     \item An unevaluated expression (e.g., `Condition$FirstGame - Condition$SecondGame`)
#'     \item A numeric matrix or data.frame of edge weights
#'     \item A language object
#'     \item NULL (defaults to the mean network)
#'   }
#' @param ... Additional parameters passed to the plotting functions.
#' @param with.mean Logical; if `TRUE`, also plots the mean for the points in the network. Default is `FALSE`.
#' @param edge.multiplier Numeric scalar to multiply the edge weights. Useful for scaling the network visualization. Default is 1.
#' @param colors Optional vector of colors for the network. If not specified, colors are chosen from the plot palette.
#'
#' @details
#' The function determines the type of the `wh` parameter and processes it accordingly:
#' \itemize{
#'   \item If `wh` is an unevaluated expression, it is captured and evaluated in the parent frame. This allows for flexible specification of group means or differences.
#'   \item If `wh` is a numeric matrix or data.frame, it is used directly as the network data.
#'   \item If `wh` is a language object, it is processed to extract the relevant network information.
#'   \item If `wh` is NULL, the mean network is plotted.
#' }
#'
#' The function updates the plot with the new network and returns the modified plot object. The ENA set is not modified in-place.
#'
#' @section Examples:
#'   See `inst/examples/example-plot-piping.R` for usage examples.
#'
#' @return The modified ENAplot object with the new network added.
#'
#' @export
add_network <- function(
  x, wh = NULL, 
  ..., 
  with.mean = F, 
  edge.multiplier = 1,
  colors = NULL
) {
  plot <- x;
  set <- plot$enaset;

  more_args <- list(...);

  wh_subbed <- substitute(wh)
  network <- colMeans(set$line.weights) * edge.multiplier;
  
  if (is.language(wh_subbed)) {
    network <- try(eval(wh_subbed, parent.frame()), silent = TRUE)
    if(inherits(network, "try-error")) {
      if(wh_subbed[[1]] == "-") {
        means <- sapply(c(wh_subbed[[2]], wh_subbed[[3]]), function(y) {
          colMeans(eval(str2lang(paste0(c("set$line.weights", y), collapse = "$"))));
        })

        network <- means[,1] - means[,2];
        named <- as.character(enquote(wh_subbed))[2];
        colors <- if(is.null(colors)) {
          plot$palette[seq.int(length(plot$plotted$points) + 1, 2)]
        } else {
          if(length(colors) < 2) {
            stop("Please provide two colors for the two groups being compared.")
          } else {
            colors
          }
        }
      }
      else {
        network <- colMeans(eval(str2lang(paste0(c("set$line.weights", wh_subbed), collapse = "$"))));
        colors <- ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors);
        named <- paste(as.character(wh_subbed)[-1], collapse = " ");
      }
    }
    else if (is.matrix(network) || is.data.frame(network) || is.numeric(network)) {
      network <- colMeans(network);
      colors <- ifelse(is.null(colors), plot$palette[length(plot$plotted$points) + 1], colors);
      named <- paste(as.character(wh_subbed)[-1], collapse = " ");
    }
  }

  more_args$enaplot = plot;
  more_args$colors = colors;
  if(is.data.frame(network) || is.matrix(network) || is.numeric(network)) {
    more_args$network = network * edge.multiplier;
    plot <- do.call(ena.plot.network, more_args);
  }

  
  # .return(set, from_plot = T, invisible = F)
  return(plot);
}


#' Add nodes to an ENA plot
#'
#' This function adds nodes to an existing ENA plot or ENA set. It can be used to customize the nodes displayed on the plot, including their size and other graphical parameters.
#'
#' @param x An \code{ENAplot} object or an ENA set containing plots.
#' @param ... Additional arguments passed to \code{ena.plot.points}, such as \code{nodes}, \code{size}, and other graphical parameters.
#' @param return_plot Logical; if \code{TRUE}, returns the modified ENA set. If \code{FALSE} (default), returns the modified plot invisibly.
#'
#' @details
#' If \code{x} is an \code{ENAplot}, the function extracts the associated ENA set and plot. Otherwise, it assumes \code{x} is an ENA set and uses the last plot in the set.
#' The nodes to be added can be specified via the \code{nodes} argument; otherwise, the default nodes from the set's rotation are used.
#' Node size can be customized via the \code{size} argument.
#'
#' The function updates the plot with the new nodes and stores the updated plot back in the ENA set.
#'
#' @return Invisibly returns the modified plot or ENA set, depending on the value of \code{return_plot}.
#'
#' @example inst/examples/example-plot-piping.R
#'
#' @export
add_nodes <- function(x, ..., return_plot = FALSE) {
  plot <- x;
  set <- plot$enaset;

  dot_args <- list(...);
  if(!is.null(dot_args$nodes)) {
    nodes <- dot_args$nodes;
  }
  else {
    nodes <- set$rotation$nodes;
  }

  node_sizes <- 1;
  if(!is.null(dot_args$size)) {
    node_sizes <- dot_args$size;
  }

  plot <- ena.plot.points(plot,
            points = as.matrix(nodes),
            texts = as.character(nodes$code),
            point.size = node_sizes,
            ...
          );

  plot$plotted$networks[[length(plot$plotted$networks) + 1]] <- list(
    nodes = nodes,
    data = NULL,
    color = NULL
  );

  # set$plots[[length(set$plots)]] <- plot

  return(plot);
}

#' Adds group means to the ENA plot.
#'
#' This function iterates over the plotted points in the ENA plot and calculates
#' the mean for each group of points. The calculated means are then added to the
#' plot as group means.
#'
#' @param x An ENA set object containing the plots.
#'
#' @return Invisibly returns the modified ENA set object with updated plots.
#'
#' @export
with_means <- function(x) {
  plot <- x;
  set <- plot$enaset;

  for(point_group in plot$plotted$points) {
    plot <- ena.plot.group(plot, point_group$data[[1]], colors = point_group$color[1])

    plot$plotted$means[[length(plot$plotted$means) + 1]] <- list(
      data = colMeans(point_group$data[[1]]),
      color = point_group$color[1]
    )
  }

  return(plot)
}


#' Adds trajectories to an ENA plot.
#'
#' This function generates trajectories for the plotted points in the ENA plot based on the specified grouping variables.
#' It supports options for jittering, animation, and scaling.
#'
#' @param x An ENA set object containing the plots.
#' @param ... Additional arguments passed to the plotting functions.
#' @param by A character vector specifying the grouping variables for the trajectories. Default is the first conversation parameter in the ENA set.
#' @param add_jitter Logical; if `TRUE`, adds jitter to the trajectory points. Default is `TRUE`.
#' @param frame Numeric; the duration of each frame in the animation. Default is 1100.
#' @param transition Numeric; the duration of the transition between frames. Default is 1000.
#' @param easing A character string specifying the easing function for the animation. Default is "circle-in-out".
#'
#' @return Invisibly returns the modified ENA set object with updated plots.
#'
#' @export
with_trajectory <- function(
  x, ...,
  by = x$`_function.params`$conversation[1],
  add_jitter = TRUE,
  frame = 1100,
  transition = 1000,
  easing = "circle-in-out"
) {
  set <- x
  if(!grepl(x = set$model$model.type, pattern = "Trajectory")) {
    stop(paste0("Unable to plot trajectories on model of type: ", set$model$model.type))
  }
  plot <- set$plots[[length(set$plots)]]

  args = list(...)

  all_steps_w_zero <- data.table(rbind(
    rep(0, length(by)),
    expand.grid(
      sapply(by, function(b) sort(unique(set$points[[b]]))),
      stringsAsFactors = F
    )
  ))
  colnames(all_steps_w_zero) <- by
  point_group_names <- seq(plot$plotted$points)
  points_cleaned <- lapply(point_group_names, function(n) {
    prepare_trajectory_data(
      points = plot$plotted$points[[n]]$data,
      by = by,
      units = plot$plotted$points[[n]]$data,
      units_by = set$`_function.params`$units,
      steps = all_steps_w_zero
    )
  })
  names(points_cleaned) <- sapply(plot$plotted$points, "[[", "color")
  points_cleaned <- rbindlist(points_cleaned, idcol = "color")

  meta_data = unique(set$meta.data)
  setkey(points_cleaned, ENA_UNIT)
  setkey(meta_data, ENA_UNIT)
  points_cleaned = meta_data[points_cleaned]
  setkeyv(points_cleaned, by)

  size = ifelse(is.null(args$size), 10, args$size)
  opacity = ifelse(is.null(args$opacity), 1, args$opacity)

  dims = as.matrix(points_cleaned[, find_dimension_cols(points_cleaned), with = F])[, 1:2]
  if(add_jitter) {
    dims[, 1] = jitter(dims[, 1])
    dims[, 2] = jitter(dims[, 2])
  }

  if(is.null(args$scale)) {
    max_abs = max(abs(dims))
    scale = c(-1*max_abs, max_abs)
  }
  else {
    scale = args$scale
  }

  ax <- list(
    range = scale, title = "",
    zeroline = TRUE, showline = FALSE,
    showticklabels = FALSE, showgrid = FALSE
  )

  #####
  ### Add to the plot
  #####
    thisPlot <- plotly::plot_ly(
        data = points_cleaned,
        x = dims[,1], y = dims[,2],
        text = ~ENA_UNIT,
        frame = as.formula(paste0("~", by)),
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = size,
          opacity = opacity,
          hoverinfo = "text",
          color = as.numeric(as.factor(points_cleaned[["color"]]))
        )
      ) |>
      plotly::layout(
        xaxis = ax,
        yaxis = ax,
        showlegend = T
      ) |>
      plotly::animation_opts(
        frame = frame,
        transition = transition,
        easing = easing,
        redraw = T
      )
  #####

  # set$model$plot <- plot
  set$plots[[length(set$plots) + 1]] <- thisPlot
  invisible(set)
}

#' Prepares trajectory data for an ENA plot.
#'
#' This function processes and prepares trajectory data for plotting in an ENA set. It handles rotation, grouping, and filling missing steps in the trajectory.
#'
#' @param x An ENA set object. If `NULL`, other parameters must be provided.
#' @param by A character vector specifying the grouping variables for the trajectory. Default is the first conversation parameter in the ENA set.
#' @param rotation_matrix A matrix used to rotate the points. Default is the rotation matrix from the ENA set.
#' @param points A data table of points to be processed. Default is the points from the ENA set.
#' @param units A data table of units corresponding to the points. Default is the trajectories or points from the ENA set.
#' @param units_by A character vector specifying the unit grouping variables. Default is the unit parameters from the ENA set.
#' @param steps A data table specifying the steps for the trajectory. If `NULL`, steps are generated automatically.
#'
#' @return A data table containing the processed trajectory data, including dimensions and metadata.
prepare_trajectory_data <- function(
  x = NULL,
  by = x$`_function.params`$conversation[1],
  rotation_matrix = x$rotation.matrix,
  points = NULL,
  units = points,
  units_by = x$`_function.params`$units,
  steps = NULL
) {
  if(is(x, "ena.set")) {
    if(is.null(points))
      points <- x$points
    if(is.null(units))
      units <- x$trajectories #points[, find_meta_cols(points), with = FALSE]
  }

  unique_unit_values <- unique(units[, c(units_by, "ENA_UNIT"), with = FALSE])

  if(!is.null(rotation_matrix)) {
    rotation_matrix = as.matrix(rotation_matrix)
    full_data <- cbind(units, as.matrix(points) %*% rotation_matrix)
  } else {
    full_data <- cbind(units, as.matrix(points))
  }
  full_data <- full_data[, unique(names(full_data)), with = FALSE]

  if(is.null(steps)) {
    all_steps_w_zero <- data.table(rbind(
      rep(0, length(by)),
      expand.grid(
        sapply(by, function(b) sort(unique(units[[b]]))),
        stringsAsFactors = F
      )
    ))
    colnames(all_steps_w_zero) <- by
  } else {
    all_steps_w_zero <- steps
  }
  all_step_data <- CJ(all_steps_w_zero[[by]], unique_unit_values$ENA_UNIT)
  colnames(all_step_data) <- c(by, "ENA_UNIT")

  dimension_col_names = colnames(points)[
                          which(sapply(points, function(col) {
                            is(col, "ena.dimension")
                          }))
                        ]
  all_step_data[, c(dimension_col_names) := 0]
  all_step_data[[by]] = as.ena.metadata(all_step_data[[by]])
  all_step_data = merge(unique_unit_values, all_step_data, by = "ENA_UNIT")
  setkey(all_step_data, "ENA_UNIT")

  filled_data = all_step_data[ , {
      by_names = names(.BY)
      user_rows = sapply(1:length(by_names), function(n) {
          full_data[[by_names[n]]] == .BY[n]
      })
      existing_row = which(rowSums(user_rows * 1) == 2)
      if(length(existing_row) > 0) {
        full_data[existing_row, c(dimension_col_names), with = FALSE]
      } else {
        prev_row = tail(full_data[ENA_UNIT == .BY$ENA_UNIT & full_data[[by]] < .BY[[by]],], 1)
        if(nrow(prev_row) == 0) {
          data.table(matrix(rep(0, length(dimension_col_names)), nrow = 1, dimnames = list(NULL, c(dimension_col_names))))
        } else {
          prev_row[, c(dimension_col_names), with = FALSE]
        }
      }

  },  by = c("ENA_UNIT", by)]
  for(col in dimension_col_names) {
    set(filled_data, j = col, value = as.ena.dimension(filled_data[[col]]))
  }
  return(filled_data)
}

#' Clears specified plots from an ENA set.
#'
#' This function removes the plots specified by their indices from the `plots` field of the ENA set.
#'
#' @param x An ENA set object containing the plots.
#' @param wh A numeric vector specifying the indices of the plots to clear. Default is all plots.
#'
#' @return Invisibly returns the modified ENA set object with the specified plots removed.
#'
#' @example inst/examples/example-plot-piping.R
#' 
#' @export
clear <- function(x, wh = seq(x$plots)) {
  if(length(wh) > 0) {
    x$plots[[wh]] <- NULL
  }
  invisible(x)
}

#' Scales the points in an ENA set.
#'
#' This function adjusts the scale of the points in the ENA set to match the range of the network.
#'
#' @param x An ENAplot object containing the set to scale.
#' @param center Unused parameter, included for compatibility.
#' @param scale A numeric value specifying the scaling factor. If `NULL`, the function will determine the scale based on the data.
#'
#' @return The modified ENAplot object with scaled points.
#'
#' @export
scale.ENAplot <- function(x, center = NULL, scale = NULL) {
  plot <- x
  set <- plot$enaset;

  point_range <- range(set$points);
  network_range <- range(set$rotation$nodes);

  if(is.null(scale)) {
    scale <- min(abs(network_range) / abs(point_range));
  }

  set$points <- set$points * scale;

  return(plot)
}

#' Updates the axis ranges of an ENA plot based on the plotted data.
#'
#' This function adjusts the x and y axis ranges of the ENA plot to ensure that all plotted points, networks, and means are visible.
#'
#' @param x An ENA plot object containing the plotted data and axis configurations.
#'
#' @return The updated ENA plot object with adjusted axis ranges.
#'
#' @export
check_range <- function(x) {
  numbers <- as.numeric(sapply(x$plotted$points, function(p) max(as.matrix(p$data))));
  means <- as.numeric(sapply(x$plotted$means, function(p) max(as.matrix(p$data))));

  network <- NULL;
  if(length(x$plotted$networks) > 0) {
    network <- abs(as.numeric(sapply(x$plotted$networks, function(nn) sapply(nn, `[`, c("x0","x1","y0","y1")))));
  }

  if(
    length(numbers) == 0 &&
    length(means) == 0
  ) {
    return(x)
  }

  curr_max = max(c(numbers, network, means))
  if(curr_max*1.2 > max(x$axes$y$range)) {
    this.max = curr_max * 1.2
    x$axes$x$range = c(-this.max, this.max)
    x$axes$y$range = c(-this.max, this.max)
    x$plot = plotly::layout(
      x$plot,
      xaxis = x$axes$x,
      yaxis = x$axes$y
    );
  } else if (curr_max < max(x$axes$y$range*0.5)) {
    this.max = curr_max * 1.2
    x$axes$x$range = c(-this.max, this.max)
    x$axes$y$range = c(-this.max, this.max)
    x$plot = plotly::layout(
      x$plot,
      xaxis = x$axes$x,
      yaxis = x$axes$y
    );
  }

  x
}

#' Display and update plot objects within a custom object
#'
#' This function updates the plots within the provided object by applying the `check_range` function to each plot.
#' It then prints the updated object using custom print options and returns the object invisibly.
#'
#' @param x An object containing a list of plots in the `plots` field.
#' @param ... Additional arguments passed to the `print` method.
#'
#' @return The updated object `x`, returned invisibly.
#'
#' @export
show <- function(x, ...) {
  # browser()
  x$plots <- lapply(x$plots, check_range)
  print(x, ..., plot = T, set = F)
  invisible(x)
}
