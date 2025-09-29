CLASS_NAMES <- list(
  data = "qe.data",
  meta = "qe.metadata",
  code = "qe.code",
  unit = "qe.unit",
  horizon = "qe.horizon"
)

WARNINGS <- list(
  data_from_vector = "Cannot transform vectors to `qe.data`",
  null_metadata = "`metadata` must be supplied as a vector of column names. No metadata classified.",
  null_codes = "`codes` must be supplied as a vector of column names. No codes classified.",
  null_units = "`units` must be supplied as a vector of column names. No units classified.",
  null_horizon = "`horizon` must be supplied as a vector of column names. No horizon classified."
)

#' Convert an object to 'qe.data' class
#'
#' This function converts an object to the 'qe.data' class. If the object is not a data.frame or matrix, it is first converted to a data.table.
#'
#' @param x An object. The object to be converted to 'qe.data' class.
#'
#' @return The modified object with the 'qe.data' class.
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   ID = 1:5,
#'   Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
#'   Age = c(25, 30, 35, 40, 45),
#'   Score = c(85, 90, 95, 80, 75)
#' )
#' dt <- as.qe.data(dt);
#' class(dt) # Should show 'qe.data' along with other classes
#'
#' @export
as.qe.data <- function(x) {
  if(!is.qe.data(x)) {
    if(is.vector(x)) {
      warning(WARNINGS$data_from_vector);
    }
    else {
      if(
         is.matrix(x) ||
        (is.data.frame(x) && !data.table::is.data.table(x))
      ) {
        x <- data.table::as.data.table(x);
      }
      class(x) <- c(CLASS_NAMES$data, class(x));
    }
  }

  # return(data.table::copy(x));
  return(x);
}

#' Convert a vector to 'qe.code' class
#'
#' This function converts a vector to the 'qe.code' class. If the vector is a factor, it is first converted to a character vector.
#'
#' @param x A vector. The vector to be converted to 'qe.code' class.
#'
#' @return The modified vector with the 'qe.code' class.
#' @examples
#' vec <- factor(c("A", "B", "C"))
#' vec <- as.qe.code(vec)
#' class(vec) # Should show 'qe.code' along with other classes
#' @export
as.qe.code <- function(x) {
  if(is.qe.code(x)) return(x);

  if(is.factor(x)) {
    x <- as.character(x);
  }
  class(x) <- c(CLASS_NAMES$code, class(x));

  return(x);
}

#' Convert a vector to 'qe.metadata' class
#'
#' This function converts a vector to the 'qe.metadata' class. If the vector is a factor, it is first converted to a character vector.
#'
#' @param x A vector. The vector to be converted to 'qe.metadata' class.
#'
#' @return The modified vector with the 'qe.metadata' class.
#' @examples
#' vec <- factor(c("A", "B", "C"))
#' vec <- as.qe.metadata(vec)
#' class(vec) # Should show 'qe.metadata' along with other classes
#' @export
as.qe.metadata <- function(x) {
  if(is.qe.metadata(x)) return(x);

  if(is.factor(x)) {
    x <- as.character(x);
  }
  class(x) <- c(CLASS_NAMES$meta, class(x));

  return(x);
}

#' Convert a vector to 'qe.unit' class
#'
#' This function converts a vector to the 'qe.unit' class. If the vector is a factor, it is first converted to a character vector.
#'
#' @param x A vector. The vector to be converted to 'qe.unit' class.
#'
#' @return The modified vector with the 'qe.unit' class.
#' @examples
#' vec <- factor(c("A", "B", "C"))
#' vec <- as.qe.unit(vec)
#' class(vec) # Should show 'qe.unit' along with other classes
#' @export
as.qe.unit <- function(x) {
  if(is.qe.unit(x)) return(x);

  if(is.factor(x)) {
    x <- as.character(x);
  }
  class(x) <- c(CLASS_NAMES$unit, class(x));

  return(x);
}

#' Convert a vector to 'qe.horizon' class
#'
#' This function converts a vector to the 'qe.horizon' class. If the vector is a factor, it is first converted to a character vector.
#'
#' @param x A vector. The vector to be converted to 'qe.horizon' class.
#'
#' @return The modified vector with the 'qe.horizon' class.
#' @examples
#' vec <- factor(c("A", "B", "C"))
#' vec <- as.qe.horizon(vec)
#' class(vec) # Should show 'qe.horizon' along with other classes
#' @export
as.qe.horizon <- function(x) {
  if(is.qe.horizon(x)) return(x);

  if(is.factor(x)) {
    x <- as.character(x);
  }
  class(x) <- c(CLASS_NAMES$horizon, class(x));

  return(x);
}

#' Check if an object is of class 'qe.data'
#'
#' This function checks if an object is of class 'qe.data'.
#'
#' @param x An object. The object to be checked.
#'
#' @return A logical value. TRUE if the object is of class 'qe.data', otherwise FALSE.
#' @examples
#' library(data.table)
#'
#' dt <- data.table(ID = 1:5)
#' class(dt) <- c("qe.data", class(dt))
#' is.qe.data(dt) # Should return TRUE
#' @export
is.qe.data <- function(x) {
  return(CLASS_NAMES$data %in% class(x));
}

#' Check if an object is of class 'qe.code'
#'
#' This function checks if an object is of class 'qe.code'.
#'
#' @param x An object. The object to be checked.
#'
#' @return A logical value. TRUE if the object is of class 'qe.code', otherwise FALSE.
#' @examples
#' dt <- 1:5
#' class(dt) <- c("qe.code", class(dt))
#' is.qe.code(dt) # Should return TRUE
#' @export
is.qe.code <- function(x) {
  return(CLASS_NAMES$code %in% class(x));
}

#' Check if an object is of class 'qe.metadata'
#'
#' This function checks if an object is of class 'qe.metadata'.
#'
#' @param x An object. The object to be checked.
#'
#' @return A logical value. TRUE if the object is of class 'qe.metadata', otherwise FALSE.
#' @examples
#' dt <- 1:5
#' class(dt) <- c("qe.metadata", class(dt))
#' is.qe.metadata(dt) # Should return TRUE
#' @export
is.qe.metadata <- function(x) {
  return(CLASS_NAMES$meta %in% class(x));
}


#' Check if an object is of class 'qe.unit'
#'
#' This function checks if an object is of class 'qe.unit'.
#'
#' @param x An object. The object to be checked.
#'
#' @return A logical value. TRUE if the object is of class 'qe.unit', otherwise FALSE.
#' @examples
#' dt <- 1:5
#' class(dt) <- c("qe.unit", class(dt))
#' is.qe.unit(dt) # Should return TRUE
#' @export
is.qe.unit <- function(x) {
  return(CLASS_NAMES$unit %in% class(x));
}

#' Check if an object is of class 'qe.horizon'
#'
#' This function checks if an object is of class 'qe.horizon'.
#'
#' @param x An object. The object to be checked.
#'
#' @return A logical value. TRUE if the object is of class 'qe.horizon', otherwise FALSE.
#' @examples
#' dt <- 1:5
#' class(dt) <- c("qe.horizon", class(dt))
#' is.qe.horizon(dt) # Should return TRUE
#' @export
is.qe.horizon <- function(x) {
  return(CLASS_NAMES$horizon %in% class(x));
}
