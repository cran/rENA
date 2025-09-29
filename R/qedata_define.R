#' Apply metadata and code transformations to a data.table
#'
#' This function applies metadata and code transformations to a data.table if provided.
#' It checks if the metadata and codes are supplied as vectors of column names.
#'
#' @param x A data.table. The data.table to be transformed.
#' @param metadata_cols A vector of column names or NULL. A vector specifying the columns for metadata transformations.
#' @param codes_cols A vector of column names or NULL. A vector specifying the columns for code transformations.
#' @param horizon_cols A vector of column names or NULL. A vector specifying the columns for horizon transformations.
#' @param units_cols A vector of column names or NULL. A vector specifying the columns for unit transformations.
#'
#' @return The modified data.table after applying the metadata and code transformations.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' dt <- define(dt, metadata = c("a"), codes = c("b"))
#' @export
define <- function(
  x,
  metadata_cols = find_meta_cols(x),
  codes_cols = find_binary_cols(x),
  horizon_cols = NULL,
  units_cols = NULL
) {
  x <- as.qe.data(x);

  do_call <- function(y, wh) {
    args <- list(x = x);
    for(u in y) args[[length(args) + 1]] <- u
    x <<- do.call(wh, args);

    return(x);
  }

  if(!is.null(metadata_cols)) {
    if(
      (is.numeric(metadata_cols) || is.character(metadata_cols)) &&
      length(metadata_cols) > 0
    ) {
      x <- do_call(metadata_cols, metadata);
    }
    else {
      warning(WARNINGS$null_metadata);
    }
  }

  if(!is.null(codes_cols)) {
    if(
      (is.numeric(codes_cols) || is.character(codes_cols)) &&
      length(codes_cols) > 0
    ) {
      x <- do_call(codes_cols, codes);
    }
    else {
      warning(WARNINGS$null_codes);
    }
  }

  if(!is.null(units_cols)) {
    if(
      (is.numeric(units_cols) || is.character(units_cols)) &&
      length(units_cols) > 0
    ) {
      x <- do_call(units_cols, units);
    }
    else {
      warning(WARNINGS$null_units);
    }
  }

  if(!is.null(horizon_cols)) {
    if(
      (is.numeric(horizon_cols) || is.character(horizon_cols)) &&
      length(horizon_cols) > 0
    ) {
      x <- do_call(horizon_cols, horizon);
    }
    else {
      warning(WARNINGS$null_horizon);
    }
  }

  invisible(x);
}

#' Reclassify specified columns as codes or list codes columns in a data.table
#'
#' This function reclassifies specified columns of a data.table to the 'qe.code' format if column names are provided.
#' If no column names are provided, it returns the names of columns that are already classified as 'qe.code'.
#'
#' @param x A data.table. The data.table containing the columns to be reclassified or checked.
#' @param ... Additional arguments specifying the names of the columns to be reclassified.
#'
#' @return The modified data.table with specified columns reclassified as 'qe.code', or a character vector of column names already classified as 'qe.unit'.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' # Reclassify columns 'a' and 'b' as 'qe.code'
#' dt <- codes(dt, "a", "b")
#' # List columns classified as 'qe.code'
#' code_columns <- codes(dt)
#' @export
codes <- function(x, ...) {
  x <- as.qe.data(x);

  if(...length() > 0) {
    dot_args <- list(...);

    # x <- reclassify(x, dot_args, as.qe.code);
    dot_args$x <- x;
    dot_args$v <- as.qe.code;
    x <- do.call(reclassify, dot_args);

    return(x);
  }
  else {
    return(colnames(x)[sapply(x, is.qe.code)]);
  }
}

#' Reclassify specified columns as metadata or list metadata columns in a data.table
#'
#' This function reclassifies specified columns of a data.table to the 'qe.metadata' format if column names are provided.
#' If no column names are provided, it returns the names of columns that are already classified as 'qe.metadata'.
#'
#' @param x A data.table. The data.table containing the columns to be reclassified or checked.
#' @param ... Additional arguments specifying the names of the columns to be reclassified.
#'
#' @return The modified data.table with specified columns reclassified as 'qe.metadata', or a character vector of column names already classified as 'qe.metadata'.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' # Reclassify columns 'a' and 'b' as 'qe.metadata'
#' dt <- metadata(dt, "a", "b")
#' # List columns classified as 'qe.metadata'
#' metadata_columns <- metadata(dt)
#' @export
metadata <- function(x, ...) {
  x <- as.qe.data(x);

  if(...length() > 0) {
    dot_args <- list(...);

    dot_args$x <- x;
    dot_args$v <- as.qe.metadata;
    x <- do.call(reclassify, dot_args);

    return(x);
  }
  else {
    return(colnames(x)[sapply(x, is.qe.metadata)]);
  }
}

#' Reclassify specified columns as units or list unit columns in a data.table
#'
#' This function reclassifies specified columns of a data.table to the 'qe.unit' format if column names are provided.
#' If no column names are provided, it returns the names of columns that are already classified as 'qe.unit'.
#'
#' @param x A data.table. The data.table containing the columns to be reclassified or checked.
#' @param ... Additional arguments specifying the names of the columns to be reclassified.
#'
#' @return The modified data.table with specified columns reclassified as 'qe.unit', or a character vector of column names already classified as 'qe.unit'.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' # Reclassify columns 'a' and 'b' as 'qe.unit'
#' dt <- units(dt, "a", "b")
#' # List columns classified as 'qe.unit'
#' unit_columns <- units(dt)
#' @export
units <- function(x, ...) {
  x <- as.qe.data(x);

  if(...length() > 0) {
    dot_args <- list(...);

    dot_args$x <- x;
    dot_args$v <- as.qe.unit;
    x <- do.call(reclassify, dot_args);
    return(x);
  }
  else {
    return(colnames(x)[sapply(x, is.qe.unit)]);
  }
}

#' Reclassify specified columns as horizon or list horizon columns in a data.table
#'
#' This function reclassifies specified columns of a data.table to the 'qe.horizon' format if column names are provided.
#' If no column names are provided, it returns the names of columns that are already classified as 'qe.horizon'.
#'
#' @param x A data.table. The data.table containing the columns to be reclassified or checked.
#' @param ... Additional arguments specifying the names of the columns to be reclassified.
#'
#' @return The modified data.table with specified columns reclassified as 'qe.horizon', or a character vector of column names already classified as 'qe.horizon'.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' # Reclassify columns 'a' and 'b' as 'qe.horizon'
#' dt <- horizon(dt, "a", "b")
#' # List columns classified as 'qe.horizon'
#' horizon_columns <- horizon(dt)
#' @export
horizon <- function(x, ...) {
  x <- as.qe.data(x);

  if(...length() > 0) {
    dot_args <- list(...);

    dot_args$x <- x;
    dot_args$v <- as.qe.horizon;
    x <- do.call(reclassify, dot_args);

    return(x);
  }
  else {
    return(colnames(x)[sapply(x, is.qe.horizon)]);
  }
}

#' @export
'@.horizon' <- horizon

#' Reclassify specified columns in a data.table
#'
#' This function reclassifies specified columns of a data.table using a provided function.
#'
#' @param x A data.table. The data.table containing the columns to be reclassified.
#' @param ... Additional arguments specifying the names of the columns to be reclassified.
#' @param v A function. The function to apply to each specified column for reclassification.
#'
#' @return The modified data.table with specified columns reclassified.
#' @examples
#' library(data.table)
#' dt <- data.table(a = 1:5, b = 6:10)
#' dt <- reclassify(dt, as.qe.code, "a", "b")
#' @export
reclassify <- function(x, v, ...) {
  wh <- list(...);
  for (i in wh) {
    data.table::set(x, j = i, value = v(x[[i]]))
  }

  return(x);
}
