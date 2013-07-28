# Compute the cost function for linear regression
computeCost <- function(X, y, theta) {
  m <- length(y)
  J = 1/(2*m) * t(X %*% theta - y) %*% (X %*% theta - y)
  return(J)
}