# Test cases for compute_SB function

test_that("compute_SB computes correct between-group scatter matrix", {
  A <- matrix(1:12, nrow = 4, byrow = TRUE)
  g <- c("A", "B", "A", "B")

  result <- compute_SB(A, g)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))
  expect_equal(round(sum(result), 2), 81) 
})

# Test cases for gmr function

test_that("gmr computes correct rotation for numeric target variable", {
  rows <- 30; 
  cols <- 4;

  # Increase the number of rows to 10
  V <- matrix(rnorm(rows * cols), nrow = rows, ncol = cols) 

  # Match the increased rows
  X <- data.frame(target = c(1, 2), predictor1 = rnorm(rows)) 

  result <- gmr(V, X)

  expect_true(is.numeric(result))
  expect_equal(length(result), 4)
})

test_that("gmr computes correct rotation for categorical target variable", {
  rows <- 30; 
  cols <- 4;
  V <- matrix(rnorm(rows * cols), nrow = rows, ncol = cols)
  X <- data.frame(target = c("A", "B"), predictor1 = rnorm(rows))

  result <- gmr(V, X)

  expect_true(is.numeric(result))
  expect_equal(length(result), 4)
})

# Test cases for get_x1_main_effect function

test_that("get_x1_main_effect computes main effect contribution correctly", {
  rows <- 30; 
  cols <- 4;
  V <- matrix(rnorm(rows * cols), nrow = rows, ncol = cols)
  X <- data.frame(x1 = rnorm(rows), x2 = rnorm(rows))

  result <- get_x1_main_effect(V, X)

  expect_true(is.matrix(result))
  expect_equal(dim(result), dim(V))
})
