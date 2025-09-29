###
#' @title ENA Rotate by regression
#'
#' @description This function allows user to provide a regression formula for rotation on x and optionally on y.
#'    If regression formula for y is not provide, svd is applied to the residual data deflated by x to get y coordinates.
#'    The regression formula uses ENA dimensions are dependent variables.
#'    The first predictor has to be two-group categorical, binary, or numerical.
#'
#' @param enaset An \code{\link{ENAset}}
#' @param params list of parameters, may include:
#'     x_var: Regression formula for x direction, such as "lm(formula=V ~ Condition + GameHalf + Condition : GameHalf)",
#'      where V always stands for the ENA points.
#'     y_var: Regression formula, similar to x_var, for y direction (optional).
#'
#' @export
#' @return \code{\link{ENARotationSet}}
ena.rotate.by.hena.regression = function(enaset, params) {
  # check arguments
  if ( !is.list(params) || is.null(params$x_var) ) {
    stop("params must be provided as a list() and provide `x_var`")
  }

  x <- params$x_var;
  y <- params$y_var;
  points <- params$points;
  fullNames <- params$fullNames;

  if(is.null(fullNames)) {
    fullNames = F;
  }

  #get points
  if(!is.null(points)) {
    p <- points
  }
  else  if (is.null(enaset$points.normed.centered)) {
    p <- as.matrix(enaset$model$points.for.projection);
  }
  else {
    p <- as.matrix(enaset$points.normed.centered);
  }

  #regress to get v1 using x
  V <- p;

  # only works using attach()
  # attach(enaset$meta.data,warn.conflicts = F)
  # v1 = eval(parse(text = x))$coefficients[2,]

  # v1 <- with(enaset$meta.data, {
  #   eval(parse(text = x))$coefficients[2,]
  # });
  # v1 <- with(enaset$model$points.for.projection, NULL, formula = x);
  v1_res <- with.ena.matrix(enaset$model$points.for.projection, {
    lm(formula(params$x_var));
  });
  v1 <- v1_res$coefficients[2,]

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
    if(fullNames) {
      warning("FullName param is likely wrong.")
      xName <- parse(text = x)[[1]][["formula"]][[3]];
    }
    else {
      xName <- all.vars(x)[2];
    }
  }

  # Save v1
  R <- matrix(c(v1), ncol = 1);
  colnames(R) <- c(paste0(xName,"_reg"));

  #deflate matrix by x dimension
  A <- as.matrix(p)
  defA <- as.matrix(A) - as.matrix(A) %*% v1 %*% t(v1)

  #if y formula is given, regress by y formula
  if (!is.null(y)) {

    # regress to get v2 vector using formula y
    V <- defA;

    # Removed attach abvove
    # v2 = eval(parse(text = y))$coefficients[2,]
    # v2 <- with(enaset$meta.data, {
    #   eval(parse(text = y))$coefficients[2,]
    # });
    # v2 <- with(enaset$model$points.for.projection, NULL, formula = y, V = V);
    v2_res <- with.ena.matrix(enaset$model$points.for.projection, {
      lm(formula(params$y_var));
    });
    v2 <- v2_res$coefficients[2,]

    #make v2 a unit vector

    norm_v2 <- sqrt(sum(v2 * v2));
    if (norm_v2 != 0) {
      v2 <- v2 / norm_v2;
    }

    #name v2 vector
    if(is.na(all.vars(y)[2])) {
      yName <- names(v2)[1]
    }
    else {
      if(fullNames) {
        warning("FullName param is likely wrong.")
        yName <- parse(text = y)[[1]][["formula"]][[3]];
      }
      else {
        yName <- all.vars(y)[2]
      }
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
