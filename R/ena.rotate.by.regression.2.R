#' @title with.ena.matrix
#' @description This function sets up a context using the provided data (typically an ENA matrix),
#' allowing the evaluation of an expression (`expr`) with access to both the matrix and
#' its metadata. Optionally, a custom matrix `V` and other arguments can be supplied.
#'
#' @param data An ENA matrix or data frame containing the data to be used.
#' @param expr An R expression to be evaluated within the context of the ENA matrix.
#' @param ... Additional arguments, including an optional custom matrix `V` and other parameters.
#'
#' @details
#' - If a custom matrix `V` is provided in `...`, it will be used; otherwise, `data` is converted to a matrix.
#' - Metadata columns are coerced to numeric if they are character vectors.
#' - The expression is evaluated with access to both the matrix (`V`) and metadata.
#'
#' @return The result of evaluating `expr` in the constructed context.
#'
#' @export
with.ena.matrix <- function(data, expr, ...) {
  dot_args <- list(...);

  # Points
  V <- NULL;
  if(length(dot_args) > 0 && !is.null(dot_args$V)) {
    print("- using custom V matrix")
    V <- dot_args$V;
  }
  else {
    V <- as.matrix(data);
  }

  # Meta data
  x <- unclass(data);
  l <- lapply(x, function(i_val) {
    # i_val <- get(i);
    if(is.character(i_val)) {
      i_val <- as.numeric(as.factor(i_val));
    }
    return(i_val);
  });

  # frm <- dot_args$frm;
  # if(!is(frm, "formula")) {
  #   frm <- formula(frm);
  # }

  l$V <- V;
  # with(l, {
  #   lm(formula = frm)
  # })

  ll <- c(l, dot_args);
  eval(substitute(expr), ll, enclos = parent.frame());
  # lm(formula = frm, data = l)
}

###
#' @title ENA Rotate by regression (second way)
#'
#' @description This function allows user to provide a regression formula for rotation on x and optionally on y.
#'    If regression formula for y is not provide, svd is applied to the residual data deflated by x to get y coordinates.
#'    The regression formula should use ENA points as major predictors and a binary or numerical variable as dependent variable.
#'    Control and interaction variables are allowed to be included as predictors in the formula.
#'
#' @param enaset An \code{\link{ENAset}}
#' @param params list of parameters, may include:
#'     x_var: Regression formula for x direction, such as "lm(formula= Condition ~ V + GameHalf + Condition : GameHalf)",
#'     where V always stands for the ENA points.
#'     y_var: Regression formula, similar to x_var for y direction (optional).
#'
#' @export
#' @return \code{\link{ENARotationSet}}
ena.rotate.by.hena.regression_2 = function( enaset, params ) {

  # check arguments
  if ( !is.list(params) || is.null(params$x_var) ) {
    stop("params must be provided as a list() and provide `x_var`")
  }

  x <- formula(params$x_var);

  if (is.null(enaset$points.normed.centered)) {
    p <- as.matrix(enaset$model$points.for.projection);
  }
  else {
    p <- as.matrix(enaset$points.normed.centered);
  }

  #get variables
  V <- as.matrix(p);
  n <- ncol(V);

  #regress to get v1 using x regression formula
  # attach(enaset$meta.data,warn.conflicts = F)
  # v1 <- eval(parse(text = x))$coefficients;
  # v1_res <- with(enaset$model$points.for.projection, NULL, formula = x);
  v1_res <- with.ena.matrix(enaset$model$points.for.projection, {
    prm_var <- params$x_var;
    prm <- if(is.character(prm_var))
        prm_var
      else
        enquote(prm_var)
    ;
    vars <- all.vars(formula(prm));
    all_exists <- sapply(vars, function(x) x == "V" || exists(x))
    if(!all(all_exists)) {
      stop(paste0("The following columns in the formula are not found in the unique metadata for the units: ", paste0(vars[!all_exists], collapse = ", ")))
    }
    lm(formula(prm));
  });
  v1 <- v1_res$coefficients;

  # remove intercept
  if(is.null(dim(v1))) {
    v1 <- v1[2:(n+1)];
  }
  else {
    v1 <- v1[2,];
  }

  # make v1  a unit vector
  norm_v1 <- sqrt(sum(v1 * v1));
  if (norm_v1 != 0) {
    v1 <- v1 / norm_v1;
  }

  # name v1 vector
  if(is.na(all.vars(x)[2])) {
    xName <- names(v1)[1];
  }
  else {
    xName <- all.vars(x)[2];
  }

  # Save v1
  R <- matrix(c(v1), ncol = 1);
  colnames(R) <- c(paste0(xName,"_reg"));

  #deflate matrix by x dimension
  A <- as.matrix(p);
  defA <- as.matrix(A) - as.matrix(A) %*% v1 %*% t(v1);

  #if y formula is given, regress by y formula
  if (!is.null(params$y_var)) {
    y <- formula(params$y_var);

    # regress to get v2 vector using formula y
    V <- defA;

    v2_res <- with.ena.matrix(enaset$model$points.for.projection, {
      prm_var <- params$y_var;
      prm <- if(is.character(prm_var))
        prm_var
      else
        enquote(prm_var)
      ;
      vars <- all.vars(formula(prm));
      all_exists <- sapply(vars, function(x) x == "V" || exists(x))
      if(!all(all_exists)) {
        stop(paste0("The following columns in the formula are not found in the unique metadata for the units: ", paste0(vars[!all_exists], collapse = ", ")))
      }
      lm(formula(prm));
    });
    v2 <- v2_res$coefficients;
    v2 <- v2[2:length(v2)];

    #make v2 a unit vector
    norm_v2 <- sqrt(sum(v2 * v2));

    if (norm_v2 != 0) {
      v2 <- v2 / norm_v2;
    }

    #name v2 vector
    if(is.na(all.vars(y)[2])) {
      yName <- names(v2)[1];
    }
    else {
      yName <- all.vars(y)[2];
    }

    # save both v1 and v2
    R <- cbind(v1, v2);
    colnames(R) <- c(paste0(xName,"_reg"), paste0(yName,"_reg"));

    #deflat by v2
    defA <- as.matrix(defA) - as.matrix(defA) %*% v2 %*% t(v2);
  }

  # get svd for deflated points
  svd_result <- prcomp(defA, retx=FALSE, scale=FALSE, center=FALSE, tol=0);
  svd_v <- svd_result$rotation;

  # Merge rotation vectors
  vcount <- ncol(R);
  colNamesR <- colnames(R);
  combined <- cbind(R, svd_v[, 1:(ncol(svd_v) - vcount)]);
  colnames(combined) <- c(
    colNamesR,
    paste0("SVD", ((vcount + 1):ncol(combined)))
  );

  #create rotation set
  rotation_set <- ENARotationSet$new(
    node.positions = NULL,
    rotation = combined,
    codes = enaset$rotation$codes,
    eigenvalues = NULL
  )

  return(rotation_set);
}

