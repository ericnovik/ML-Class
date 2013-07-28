mapFeature <- function(X1, X2, degree=6) {
  #   Feature mapping function to polynomial features
  #
  #   mapFeature(X1, X2) maps the two input features
  #   to degree polynomial feaures
  #
  #   Returns a new feature array with more features, comprising of 
  #   X1, X2, X1.^2, X2.^2, X1*X2, X1*X2.^2, etc..
  #
  #   Inputs X1, X2 must be the same size
  #
  
  # For 6 degree poly: 28 features
  num.features <- sum((degree + 1):1)
  out <- matrix(1, length(X1), num.features)
  
  # need to start building features from column 2. columns 1 is all ones.
  indx <- 2
  for (i in 1:degree) {
    for (j in 0:i) {
      out[, indx] <- X1^(i-j) * X2^j
      indx <- indx + 1
    }
  }
  return(out)
}
