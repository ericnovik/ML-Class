plotDecisionBoundary <- function(theta, X, y, degrees=6) {
  
  if (dim(X)[2] <= 3) {
    slope     <- theta[2] / -theta[3]
    intercept <- theta[1] / -theta[3]
  
    qplot(X[, 2], X[, 3], colour=as.factor(y)) + 
      geom_abline(intercept = intercept, slope=slope, 
                  size=2, col='blue', alpha=1/2)  
  }
  else {
    u <- seq(-1, 1.5, length.out=50)
    v <- seq(-1, 1.5, length.out=50)
    z <- matrix(0, length(u), length(v))
    
    for (i in 1:length(u)) {
      for (j in 1:length(v)) { 
        z[i, j] <- mapFeature(u[i], v[j], degrees) %*% theta
      }
    }
    plot(X[, 2], X[, 3], col=y+2, pch=20)
    contour(u, v, z, nlevels=1, add=TRUE, col='blue', lwd=2,
            drawlabels=FALSE)
  }
}
