gradFunction <- function (theta, X, y, lambda=0) {
  # Purpose: Computes Logistics Regression Gradient with L2 Norm
  # Args:
  #       theta: Parametor vector
  #       X: Design Martix with the first colunm of ones
  #       y: response vector
  #       lambda: regularization constant
  # Returns:
  #       grad: gradient vector
  
  m <- length(y)
  n <- length(theta)
  h <- invlogit(X %*% theta)

  # we are not regularizing the intercept term
  grad.zero <- 1/m * t(h-y) %*% X[, 1]
  grad.non.zero <- 1/m * t(h-y) %*% X[, 2:n] + lambda/m * theta[2:n]
  grad <- c(grad.zero, grad.non.zero)
  
  return(grad)
}