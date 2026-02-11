#' Accumulate Connection Counts for ENA
#'
#' This function takes a data.frame and accumulates co-occurrences of codes within specified units and conversations (horizon), preparing it for ENA. It's designed to be used with pipes (`|>`)..
#'
#' @param x A data.frame or similar object containing the data to be analyzed.
#' @param units A character vector specifying the columns that define the units of analysis.
#' @param codes A character vector specifying the columns that contain the codes for co-occurrence analysis.
#' @param horizon A character vector specifying the columns that define the conversational boundaries (horizon).
#' @param ... Additional arguments passed to underlying accumulation functions.
#' @param ordered A logical value. If TRUE, creates ordered networks (A -> B is different from B -> A). Defaults to FALSE.
#' @param binary A logical value. If TRUE, connection counts are binarized (0 or 1). Defaults to TRUE.
#'
#' @return An ena.set object containing the accumulated connection counts and metadata.
#' @export
#'
#' @examples
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon)
#'
accumulate <- function(
    x,
    units = rENA::units(x),
    codes = rENA::codes(x),
    horizon = rENA::horizon(x),
    ...,
    ordered = FALSE,
    binary = TRUE
) {
  # set <- ena.accumulate.data.file(
  #   file = x,
  #   units.by = units,
  #   conversations.by = horizon,
  #   codes = codes,
  #   ...
  # )
  args <- list(...)
  force(units);
  force(codes);
  force(horizon);

  hoo_rules <- list(
    str2lang(paste0("(", paste0(sapply(horizon, function(cb) paste0(cb, " %in% UNIT$", cb)), collapse = " & "), ")"))
  )
  contexts <- tma::contexts(
    x,
    units_by = make.names(units),
    hoo_rules = hoo_rules,
    split_rules = function(unit, unit_context) {
      split(unit_context, by = horizon)
    }
  )

  win_wgts <- if(is.null(args$tensor)) {
    args$default_window <- if (is.null(args$default_window)) 1 else args$default_window;
    args$default_weight <- if (is.null(args$default_weight)) 1 else args$default_weight;
    tma::context_tensor(
      df = x,
      sender_cols = args$tma_ground_cols,
      receiver_cols = args$tma_response_cols,
      mode_column = ifelse(is.null(args$mode_column), tma::ATTR_NAMES$CONTEXT_ID, args$mode_column),
      default_window = args$default_window,
      default_weight = args$default_weight
    )
  }
  else {
    args$tensor
  }

  # args$ordered <- if (is.null(args$ordered)) TRUE else FALSE
  # browser()
  set <- tma::accumulate(
    context_model = contexts,
    # multidim_arr = multidim_arr,
    tensor = win_wgts,
    # time_column = args$time_column,
    codes = make.names(codes),
    ordered = ordered,
    binary = binary
  )

  set$rotation <- list(
    rotation.matrix = NULL,
    codes = codes,
    adjacency.key = sapply(colnames(as.matrix(set$connection.counts)), function(y) strsplit(y, "\\s?&\\s?")[[1]], simplify = T),
    node.positions = NULL,
    eigenvalues = NULL,
    centervec = NULL
  )

  return(set)
}

##' Build a Complete ENA Model
#'
#' This function applies a full ENA modeling pipeline to accumulated data. It is a convenience wrapper that chains together normalization, centering, rotation, projection, and optional optimization. Each step can be customized by supplying an alternative function.
#'
#' @param data An `ena.set` object, typically the result of `accumulate()`.
#' @param ... Additional arguments passed to the rotation function specified by `rotate_fun`.
#' @param normalize A function to normalize the connection counts. Defaults to `sphere_norm`.
#' @param center_with A function to center the normalized data. Defaults to `center`.
#' @param rotate_with A function to perform the rotation (e.g., SVD). Defaults to `rotate`.
#' @param project_with A function to project the points into the rotated space. Defaults to `project`.
#' @param optimize_with A function to optimize node positions. Defaults to `optimize`. Can be set to `NULL` or `FALSE` to skip.
#' @param rotate_fun The specific rotation function to be used by `rotate_with`. Defaults to `ena.rotate.by.generalized`.
#' @param rotate_params A list of additional parameters to pass to the `rotate_fun`.
#'
#' @return An `ena.set` object with a complete ENA model, including projected points and node positions.
#' @export
#'
#' @examples
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   model()
model <- function(
  data, ...,
  normalize = sphere_norm,
  center_with = center,
  rotate_with = rotate,
  project_with = project,
  optimize_with = optimize,
  # Rotation specific parameters
  rotate_fun = ena.rotate.by.generalized,
  rotate_params = list()
) {
  # if(is(data, "ena.ordered.set")) {
  #   if(requireNamespace("ona", quietly = TRUE)) {
  #     x <- ona::model(data, ...);
  #   } else {
  #     stop("The 'ona' package is required for ordered ENA modeling. Please install it from CRAN.");
  #   }
  # }
  # else {
    x <- normalize(data)
    x <- center_with(x)

    if (length(rotate_params) > 1) {
      x <- do.call(rotate_with, list(x, wh = rotate_fun, by = unlist(rotate_params)))
    }
    else {
      x <- rotate_with(x, wh = rotate_fun, by = rotate_params)
    }

    x <- project_with(x)

    if (!is.null(optimize_with) && !isFALSE(optimize_with)) {
      x <- optimize_with(x)
    }
  # }

  return(x)
}

##' Apply Spherical Normalization to ENA Data
#'
#' This function applies spherical normalization to the connection counts in an `ena.set` object or to a raw matrix of connection counts. Normalization is a key step before centering and rotation in ENA.
#'
#' @param x An `ena.set` object or a numeric matrix of connection counts.
#' @param add.meta A logical value. If `TRUE` (the default), metadata from the `ena.set` is preserved and included in the output. This parameter is ignored if `x` is a matrix.
#'
#' @return If `x` is an `ena.set`, it returns the modified `ena.set` with a new `line.weights` matrix and an updated `centervec` in the `rotation` object. If `x` is a matrix, it returns a matrix of normalized line weights.
#' @export
#'
#' @examples
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   sphere_norm()
sphere_norm <- function(x, add.meta = TRUE) {
  x_ <- NULL
  names_ <- NULL
  meta_ <- NULL

  # verify that the connection.counts exist

  if (is(x, "ena.set")) {
    if (is.null(x$connection.counts)) {
      stop("Connection counts are missing.")
    }

    x_ <- as.matrix(x$connection.counts)
    names_ <- colnames(x_)
    if (isTRUE(add.meta)) {
      meta_ <- x$meta.data
    }

    x$line.weights <- fun_sphere_norm(x_)
    colnames(x$line.weights) <- names_

    x$line.weights <- as_line_weights_matrix(x$line.weights, meta_)
    x$rotation$centervec <- colMeans(x$line.weights)
  }
  else {
    x_ <- as.matrix(x);
    names_ <- colnames(x_);
    x <- fun_sphere_norm(x_);
    colnames(x) <- names_;
  }

  return(x)
}


as_points_matrix <- function(x, metadata = NULL) {
  x_ <- data.table::as.data.table(x)
  for (i in seq(ncol(x_))) {
    set(x_,
      j = i,
      value = as.ena.co.occurrence(x_[[i]])
    )
  }

  if (!is.null(metadata)) {
    x_ <- cbind(metadata, x_)
  }

  class(x_) <- c("ena.points", "ena.matrix", class(x_))

  return(x_)
}

as_line_weights_matrix <- function(x, metadata = NULL) {
  line.weights.dt <- data.table::as.data.table(x)
  for (i in seq(ncol(line.weights.dt))) {
    set(line.weights.dt,
      j = i,
      value = as.ena.co.occurrence(line.weights.dt[[i]])
    )
  }

  x_ <- line.weights.dt
  if (!is.null(metadata)) {
    x_ <- cbind(metadata, line.weights.dt)
  }

  class(x_) <- c("ena.line.weights", "ena.matrix", class(line.weights.dt))

  return(x_)
}

as_rotation_matrix <- function(x) {
  x_ <- data.table::as.data.table(x, keep.rownames = "codes")
  for (i in seq(ncol(x_))) {
    if (i == 1) {
      set(x_, j = i, value = as.ena.metadata(x_[[i]]))
    } else {
      set(x_, j = i, value = as.ena.dimension(x_[[i]]))
    }
  }
  class(x_) <- c("ena.rotation.matrix", class(x_))

  return(x_)
}

as_nodes_matrix <- function(x, rows, cols = NULL, cls = "ena.matrix") {
  x_ <- data.table::data.table(rows[[1]], x)
  rownames(x_) <- rows[[1]]

  if (!is.null(cols)) {
    colnames(x_) <- c(names(rows), cols)
  }

  for (i in seq(ncol(x_))) {
    if (i == 1) {
      set(x_, j = i, value = as.ena.metadata(x_[[i]]))
    } else {
      set(x_, j = i, value = as.ena.dimension(x_[[i]]))
    }
  }

  class(x_) <- c(cls, class(x_))

  return(x_)
}

##' Center ENA Data
#'
#' This function centers the line weights of an `ena.set` by subtracting the mean of each connection from all units. This is a standard step in preparing data for rotation.
#'
#' @param x An `ena.set` object (typically after `sphere_norm()`) or a numeric matrix.
#' @param add.meta A logical value. If `TRUE` (the default), metadata is preserved. Ignored if `x` is a matrix.
#'
#' @return If `x` is an `ena.set`, it returns the modified `ena.set` with the centered data stored in `x$model$points.for.projection`. If `x` is a matrix, it returns a centered matrix.
#' @export
#'
#' @examples
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   sphere_norm() |>
#'   center()
center <- function(x, add.meta = TRUE) {
  x_ <- NULL
  names_ <- NULL
  meta_ <- NULL

  if (is(x, "ena.set")) {
    # make sure the line weights exist and are a matrix
    if (is.null(x$line.weights)) {
      stop("Missing line.weights on the provided ENA set. This is typically created using the 'accumulate' and 'sphere_norm' functions.")
    }

    x_ <- as.matrix(x$line.weights)
    is_unordered_set <- ncol(x_) == choose(length(x$rotation$codes), 2)
    names_ <- apply(tma::adjacency_key(x$rotation$codes, is_unordered_set), 2, paste, collapse = " & ")
    if (isTRUE(add.meta)) {
      meta_ <- x$meta.data
    }

    x$model$points.for.projection <- center_data_c(as.matrix(x_))
    colnames(x$model$points.for.projection) <- names_

    x$model$points.for.projection <- as_points_matrix(x$model$points.for.projection, meta_)
  }
  else {
    x_ <- as.matrix(x);
    names_ <- colnames(x_);
    x <- center_data_c(x_);
    colnames(x) <- names_;
  }


  return(x)
}

#' Rotate ENA Data
#'
#' Rotates ENA data using a specified rotation function (default: SVD), optionally using formulas or grouping variables.
#'
#' @param x An \code{ena.set} object to be rotated.
#' @param ... Optional formulas or additional arguments for rotation.
#' @param wh Function to use for rotation (default: \code{ena.svd}).
#'
#' @return The rotated \code{ena.set} object with updated rotation matrices.
#' @export
#'
#' @examples
#' # Assuming 'set' is an ena.set object:
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   sphere_norm() |>
#'   center() |>
#'   rotate()
rotate <- function(
  x,
  ...,
  wh = ena.rotate.by.generalized
) {
  x_ <- NULL
  names_ <- NULL
  codes_ <- NULL
  meta_ <- NULL
  dot_args <- list(...)

  if (is(x, "ena.set")) {
    # Make sure points.for.projection exists
    if (is.null(x$model$points.for.projection)) {
      stop("Missing `points.for.projection` on the provided ENA set. This is typically created using ?center()")
    }

    if (!is.null(dot_args$add.meta) && isTRUE(dot_args$add.meta)) {
      meta_ <- x$meta.data
    }
  }
  else {
    # Construct ENAset-like list from provided matrix
    x_ <- as.matrix(x);
    names_ <- colnames(as.matrix(x_));
    codes_ <- unique(unlist(strsplit(names_, " & ")));

    x <- list(
      model = list(
        points.for.projection = x_
      ),
      rotation = list(
        codes = codes_
      )
    )
  }

  by_vals <- NULL

  # if (length(dot_args) == 0) {
  #   wh <- ena.svd
  # }
  # else {
    dot_formulas <- sapply(dot_args, function(d) {
      d2 <- tryCatch(
        {
          d3 <- as.formula(d)
          TRUE
        },
        error = function(e) FALSE
      )
      return(d2)
    })
    if (any(dot_formulas)) {
      if (all(dot_formulas)) {
        wh <- ena.rotate.by.hena.regression_2
        by_vals <- list(params = dot_args)
        names(by_vals$params) <- c("x_var", "y_var")[seq_along(by_vals)]
      }
      else {
        stop("If rotating using a formula, all must be formulas")
      }
    }
    else {
      # Means rotation?
      by_vals <- list();
      if (!is.null(dot_args$params)) {
        by_vals <- dot_args$params
      }
      else if (!is.null(dot_args$by$params)) {
        by_vals <- dot_args$by$params
      }
      else {
        by_vals <- list(
          x_var = NULL,
          y_var = NULL
        )

        first_meta <- setdiff(colnames(x$connection.counts)[find_meta_cols(x$connection.counts)], c("QEUNIT", "ENA_UNIT"))[1]
        # args$rotate.by is a list of columns to subset from accum$connection.counts
        by_vals$x_var <- x$connection.counts[, ..first_meta, drop = FALSE];
      }
    }
  # }

  x$rotation <- do.call(wh, list(enaset = x, params = by_vals))

  # Ensure x$rotation is a list with required elements
  if (!is.list(x$rotation)) {
    stop("Rotation function did not return a list as expected.")
  }

  # Only extract elements that exist in the returned list
  rotation_elements <- c("eigenvalues", "codes", "node.positions", "rotation")
  x$rotation <- x$rotation[intersect(rotation_elements, names(x$rotation))]

  if (!is.null(x$rotation$rotation)) {
    x$rotation.matrix <- as_rotation_matrix(x$rotation$rotation)
    x$rotation$rotation.matrix <- x$rotation.matrix
    x$rotation$rotation <- NULL
  }
  else {
    x$rotation.matrix <- NULL
    x$rotation$rotation.matrix <- NULL
  }

  x$rotation.matrix <- as_rotation_matrix(x$rotation$rotation)
  x$rotation$rotation.matrix <- x$rotation.matrix
  x$rotation$rotation <- NULL

  return(x)
}

##' Project ENA Points onto Rotated Space
#'
#' This function projects ENA points onto the rotated space using the rotation matrix.
#' Optionally, metadata can be included in the resulting points matrix.
#'
#' @param x An \code{ena.set} object containing the points for projection and rotation matrix.
#' @param rotation Optional. A rotation matrix to use for projection if \code{x} is not an \code{ena.set}.
#' @param add.meta Logical. If \code{TRUE} (default), metadata will be included in the output.
#'
#' @return The input \code{ena.set} object with the projected points matrix (and metadata if requested).
#' @export
#'
#' @examples
#' # Assuming 'set' is an ena.set object:
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   sphere_norm() |>
#'   center() |>
#'   rotate() |>
#'   project()
project <- function(x, rotation = NULL, add.meta = TRUE) {
  meta_ <- NULL

  if (is(x, "ena.set")) {
    points <- as.matrix(x$model$points.for.projection) %*% as.matrix(x$rotation.matrix);

    if (isTRUE(add.meta)) {
      meta_ <- x$meta.data;
    }
    x$points <- as_points_matrix(points, meta_);

    var_rot_data <- var(points)
    diagonal_variance <- as.vector(diag(var_rot_data))
    x$model$variance <- diagonal_variance / sum(diagonal_variance)
    names(x$model$variance) <- colnames(x$rotation$rotation.matrix)[-1]

    return(x)
  }
  else {
    if(is.null(rotation)) {
      stop("When providing a matrix, a rotation matrix must also be provided")
    }

    points <- as.matrix(x) %*% as.matrix(rotation);
    return(points);
  }
}


##' Optimize Node and Centroid Positions in ENA Set
#'
#' This function computes and assigns node positions and centroids for an ENA set object
#' using the current points and rotation information.
#'
#' @param x An \code{ena.set} object for which to optimize node and centroid positions.
#' @param weights Optional. A numeric matrix of connection weights. If provided, the function will use this matrix instead of the connection counts from the \code{ena.set}.
#'
#' @return The input \code{ena.set} object with updated node and centroid positions.
#' @export
#'
#' @examples
#' # Assuming 'set' is an ena.set object:
#' data(RS.data)
#'
#' codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
#'            "Client.and.Consultant.Requests", "Design.Reasoning",
#'            "Collaboration")
#' units <- c("Condition", "UserName")
#' horizon <- c("Condition", "GroupName")
#' enaset <- RS.data |>
#'   accumulate(units, codes, horizon) |>
#'   sphere_norm() |>
#'   center() |>
#'   rotate() |>
#'   project() |>
#'   optimize()
optimize <- function(x, weights = NULL) {
  if(!is(x, "ena.set")) {
    if(is.null(weights)) {
      stop("When providing a matrix, weights must also be provided")
    }

    x_ <- x;
    x <- list(
      points = x_,
      line.weights = weights,
      rotation = list(
        codes = unique(unlist(strsplit(colnames(as.matrix(weights)), " & ")))
      )
    )
  }

  points = as.matrix(x$points);
  weights = as.matrix(x$line.weights);
  if(is(x, "ena.ordered.set")) {
    positions <- directed_node_positions(weights, points, ncol(points));
    x$rotation$nodes <- as_nodes_matrix(positions$nodes, list("code" = x$rotation$codes), cols = colnames(as.matrix(x$points)), cls = "ena.nodes")
  }
  else {
    # browser()
    positions <- lws_lsq_positions(weights, points, ncol(points));
    x$rotation$nodes <- as_nodes_matrix(positions$nodes, list("code" = x$rotation$codes), cols = colnames(as.matrix(x$points)), cls = "ena.nodes")
  }

  x$model$centroids <- as_nodes_matrix(positions$centroids, rows = list("ENA_UNIT" = x$points$ENA_UNIT), cols = colnames(as.matrix(x$points)))

  return(x)
}
