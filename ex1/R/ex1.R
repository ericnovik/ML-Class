source('Rstart.R')

setwd('~/Dropbox/Learning/Stanford/ML/ex1/R/')
source('warmUpExercise.R')
source('plotData.R')
source('computeCost.R')
source('gradientDescent.R')
require(rgl)


## Machine Learning Online Class - Exercise 1: Linear Regression

## ==================== Part 1: Basic Function ====================
cat('Running warmUpExercise ...')
cat('5x5 Identity Matrix: ')
warmUpExercise()

##  ======================= Part 2: Plotting =======================
cat('Plotting Data ...')
data <- read.csv('ex1data1.txt', header = FALSE)
colnames(data) <- c('PopulationSize', 'Profit')

X <- data[, 1] 
y <- data[, 2]

m <- length(y) # number of training examples

# Plot Data
# Note: You have to complete the code in plotData.m
plotData(X, y)
## =================== Part 3: Gradient descent ===================
cat('Running Gradient Descent ...')

# Add a column of ones to X (design matrix)
X <- cbind(rep(1, length(X)), X)

# initialize fitting parameters
theta <- rep(0, 2) 

# Some gradient descent settings
# alpha between 0.01 and 0.02 seems optimal
iterations <- 1500
alpha <- 0.01

# compute and display initial cost
computeCost(X, y, theta)[1]

# run gradient descent
theta <- gradientDescent(X, y, theta, alpha, iterations, animate=TRUE);

# todo: implement the same procedure using Newton Raphson method
# todo: implement the same using optim() or optimx()

# print theta to screen
cat('Theta found by gradient descent: ', theta$theta)

# compare to the lm() function output
lm.fit <- lm(y ~ X[, 2])
cat('Theta found by lm() function: ', coef(lm.fit))

# compare to the normal eqautions, should be identical to the above
cat('Theta found by normal equations: ', solve(t(X) %*% X) %*% t(X) %*% y)

# plot the error function
plot(theta$J, type='l', main = 'Linear Regression Error Function',
     ylab = 'Objective', xlab = 'Iteration #', col = 'red')

# Plot the linear fit
plotData(X[, 2], y)
abline(theta$theta[1], theta$theta[2], col='blue')
abline(reg=lm.fit, col='green')

# Predict values for population sizes of 35,000 and 70,000
# Need a confidence interval for the predictions
predict1 <- c(1, 3.5) %*% theta$theta
cat('For population = 35,000, we predict a profit of:', predict1 * 10000)

predict2 <- c(1, 7) %*% theta$theta
cat('For population = 70,000, we predict a profit of:', predict2 * 10000)

# ============= Part 4: Visualizing J(theta_0, theta_1) =============
cat('Visualizing J(theta_0, theta_1)...')
# 
# Grid over which we will calculate J
theta0_vals <- seq(-10, 10, length.out=100)
theta1_vals <- seq(-1, 4, length.out=100);

# initialize J_vals to a matrix of 0's
J_vals <- matrix(0, length(theta0_vals), length(theta1_vals))

# Fill out J_vals (X must be design matrix)
for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], theta1_vals[j])    
    J_vals[i,j] <- computeCost(X, y, t)
  }
}

# error surface plot from defaulr library
persp(theta0_vals, theta1_vals, J_vals, 
      theta=40, phi=15, expand=0.65,
      col='lightblue', shade=0.3, ltheta=10,
      ticktype='detailed',
      xlab='beta_0',
      ylab='beta_1',
      zlab = 'Cost')

# error surface plot from RGL
persp3d(theta0_vals, theta1_vals, J_vals,
        col  = "lightblue",
        xlab = 'beta_o',
        ylab = 'beta_1',
        zlab = 'Cost')

# countour plot
filled.contour(theta0_vals, theta1_vals, log(J_vals),
               color.palette = terrain.colors,
               plot.title = title(main = "Countour Plot of Parameter Space",
               xlab = expression(beta[0]), 
               ylab = expression(beta[1])),
               key.title = title(main="Log Cost\nFunction"))