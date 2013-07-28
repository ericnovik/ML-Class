setUpPlotRegion <- function(num_iters) {
  plot(1, 
       main = 'Linear Regression Gradient Descent', 
       type = 'n', # do not display any points, just setup the plotting region
       ylab = 'Cost Function J(\\theta)', 
       xlab = 'Iteration #', 
       xlim = c(0, num_iters),
       ylim = c(150, 2000))
}

gradientDescent <- function(X, y, theta, alpha, num_iters, 
                            debug = FALSE, animate = FALSE) {
  m <- length(y)
  J_history <- rep(0, num_iters)
  
  # set up the plotting area for animation
  if (animate) setUpPlotRegion(num_iters)
  
  for (i in 1:num_iters) {
    theta <- theta - alpha/m * t(X) %*% (X %*% theta - y)
    J_history[i] <- computeCost(X, y, theta)[1]
    
    # print some debugging information
    if (debug) {
      cat('Iteration: ', i, '\n')
      cat('Theta: ', theta, '\n')
      cat('Cost Function: ', J_history[i], '\n')
    }
    
    # display the first point, the last point, and every 10th point
    if (animate & (i==1 | i==num_iters | !(i %% 10))) {
      points(i, J_history[i], col='red', pch=20)
      Sys.sleep(0.1)
    }
  }
  return(list(theta = theta, J = J_history))
}
