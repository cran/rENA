#' Compute Between-Group Scatter Matrix
#'
#' This function calculates the between-group scatter matrix (\code{SB}) for a given numeric matrix and grouping variable.
#'
#' @param A A numeric matrix of dimensions \code{m x n}, where rows represent observations and columns represent features.
#' @param g A grouping variable of length \code{m}, either a factor or a character vector, indicating group membership for each observation.
#'
#' @return A numeric matrix representing the between-group scatter matrix (\code{SB}).
#'
#' @details
#' The function computes the total mean of the matrix \code{A} and the mean for each group defined by \code{g}.
#' It then calculates the between-group scatter matrix by summing the outer product of the mean differences, weighted by the group sizes.
#'
#' @examples
#' # Example usage:
#' A <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' g <- factor(c("A", "B", "A", "B", "A"))
#' SB <- rENA:::compute_SB(A, g)
compute_SB <- function(A, g) {
  if (!is.matrix(A)) stop("A must be a numeric matrix.")
  if (length(g) != nrow(A)) stop("g must have the same length as number of rows in A.")

  g <- as.factor(g);
  groups <- levels(g);
  n_features <- ncol(A);
  m <- nrow(A);

  # Total mean
  mu_total <- colMeans(A);

  # Initialize matrices
  SB <- matrix(0, n_features, n_features);

  for (grp in groups) {
    idx <- which(g == grp);
    A_grp <- A[idx, , drop = FALSE];
    n_g <- nrow(A_grp);
    mu_g <- colMeans(A_grp);

    # Between-group component
    mean_diff <- matrix(mu_g - mu_total, ncol = 1);
    SB <- SB + n_g * (mean_diff %*% t(mean_diff));
  }

  return(SB);
}


#' Generalized Means Rotation (GMR)
#'
#' Computes the generalized means rotation for a given set of ENA points and predictor variables.
#'
#' @param V A matrix containing ENA set points for projection.
#' @param X A data frame containing all predictor variables, with the first column as the target variable.
#'
#' @return A numeric vector representing the rotation.
#'
#' @details
#' If \code{X} has only one column, a linear model is fit between \code{V} and the single predictor.
#' Otherwise, the main effect of the first predictor is extracted using \code{get_x1_main_effect}.
#' Singular value decomposition (SVD) is then performed, and the first right singular vector is used
#' to project the data. A linear model is fit to the projected data, and the coefficients are normalized
#' to produce the rotation vector.
#'
#' @examples
#' \dontrun{
#' V <- matrix(rnorm(100), ncol = 5)
#' X <- data.frame(target = rnorm(20), predictor1 = rnorm(20), predictor2 = rnorm(20))
#' r <- gmr(V, X)
#' }
#'
#' @seealso \code{\link{get_x1_main_effect}}
#' @export
# generalized means rotation
# V - matrix, ENA set points for projection
# X - data frame containing all predictor variables, first as target
gmr <- function(V,X) {
  # matrix, ENA set points for projection
  # data frame containing all predictor variables, first as target
  Vx <- NULL; # main effect of X1 adjusted for covariates
  r <- NULL; # return direction
  Vx1 <- NULL; # main effect of X1 without adjustment
  target <- X[[1]]          # always returns the column itself
  
  if (is.list(target)) {    # flatten if it's a list-column
    target <- unlist(target, recursive = FALSE)
  }
  target <- as.vector(target)  # ensure atomic

  model <- lm(V ~ target)
  #model <- lm(V ~ X[, 1]); # simple linear model on X[1]
  Vx1 <- model$fitted.values;
  if(ncol(X)==1) { # simple linear model if there is no covariates
    Vx <- Vx1;
  }
  else { # Lasso model adjusted for covariates
    Vx <- get_x1_main_effect(V,X);
  }
  if (is.numeric(target)) { # compute direction for numerical variable
    # Reuse the coefficients from the initial model instead of rebuilding
    beta <- coef(model)[2,];  # Second coefficient is for the slope
    r <- beta / sqrt(sum(beta^2));
  }
  else {
    sb <- compute_SB(Vx, target);

    r <- svd(sb)$v[, 1];

  }
  # project r to span of row vectors of V
  #model <- lm(r ~ t(V) + 0)
  #r<- Vx1 <- model$fitted.values;
  #r <- t(V) %*% coef(lm(r ~ t(V) + 0));    # Projection: r ~ V^T %*% beta
  #r <- r / sqrt(sum(r^2));
  attr(r, "target") <- target
  attr(r, "Vx1") <- Vx1# target contribution
  return(r);
}

#' Extract Main Effect Contribution of the First Predictor Using Elastic Net
#'
#' This function fits a multivariate elastic net regression model (using \code{glmnet})
#' to estimate the main effect contribution of the first predictor variable (\code{x1})
#' in the provided data frame \code{X} for each response variable in \code{V}.
#' The main effect is forced into the model by setting its penalty factor to zero.
#'
#' @param V A numeric matrix of response variables (dimensions: observations x responses).
#' @param X A data frame containing all predictor variables. The first column is treated as the target predictor (\code{x1}).
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso). Default is 1.
#' @param lambda The value of the regularization parameter to use. Can be \code{"lambda.min"}, \code{"lambda.1se"}, or a specific numeric value. Default is \code{"lambda.min"}.
#'
#' @return A numeric matrix of the same dimensions as \code{V}, where each column contains the estimated main effect contribution of \code{x1} for the corresponding response variable.
#'
#' @details
#' The function constructs a model matrix including all main effects and two-way interactions.
#' It then fits a multivariate elastic net model using \code{cv.glmnet}, forcing the inclusion of the main effect of \code{x1} by setting its penalty factor to zero.
#' The resulting coefficients for \code{x1} are used to compute its contribution to the fitted values for each response variable.
#'
#' @import glmnet
#' @examples
#' \dontrun{
#' set.seed(123)
#' V <- matrix(rnorm(100 * 2), ncol = 2)
#' X <- data.frame(x1 = rnorm(100), x2 = sample(letters[1:3], 100, TRUE))
#' result <- get_x1_main_effect(V, X)
#' }
#' @export
get_x1_main_effect <- function(V, X, alpha = 1, lambda = "lambda.min") {
  # assume the first variable is the target variable
  x1_name <- colnames(X)[1];

  # Create model matrix (handles factors correctly)
  mm <- model.matrix(~ .^2, data = X);

  # This pattern matches x1_name at the start of the string, not followed by a colon
  x1_cols <- grep(paste0("^", x1_name, "$|^", x1_name, "(?=[^:])"), colnames(mm), perl = TRUE);

  # Create penalty factors (0 for x1 terms to force inclusion, 1 for others)
  penalty.factor <- rep(1, ncol(mm));

  # Don't penalize x1 terms
  penalty.factor[x1_cols] <- 0
  fit <- cv.glmnet(x = mm, y = V, family = "mgaussian", alpha = alpha, penalty.factor = penalty.factor);
  coefs_list <- coef(fit, s = lambda);

  # Extract x1 coefficients for each response variable
  x1_coefs <- lapply(coefs_list, function(coef_mat) {
    coef_mat[x1_cols, , drop = FALSE];
  });

  # Calculate x1 main effect contribution for each response variable
  x1_contribution <- matrix(NA, nrow = nrow(V), ncol = ncol(V));

  for (i in 1:ncol(V)) {
    if (length(x1_cols) > 0) {
      temp_result <- mm[, x1_cols, drop = FALSE] %*% x1_coefs[[i]];
      x1_contribution[, i] <- as.vector(temp_result);
    }
    else {
      # If no x1 coefficients, contribution is zero
      x1_contribution[, i] <- 0;
    }
  }
  colnames(x1_contribution) <- colnames(V);

  return(x1_contribution);
}
