costFunction <- function (theta, X, y, lambda=0) {
  # Purpose: Computes Logistics Regression Gradient with L2 Norm
  # Args:
  #       theta: Parametor vector
  #       X: Design Martix with the first colunm of ones
  #       y: response vector
  #       lambda: regularization constant
  # Returns:
  #       cost: Value of the cost fucntion, given theta
  
  m <- length(y)
  n <- length(theta)
  h <- invlogit(X %*% theta)
  
  cost <- 1/m * ((t(-y) %*% log(h)) - t((1-y)) %*% log(1-h)) +
          lambda / (2*m) * sum(theta[2:n]^2)
  
  return(cost)
}