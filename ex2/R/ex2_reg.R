source('Rstart.R')
setwd('~/Dropbox/Learning/Stanford/ML/ex2/R/')

# support functions
source('mapFeature.R')
source('costFunction.R')
source('gradFunction.R')
source('plotDecisionBoundary.R')

# support packages
require(arm)
require(glmnet)
require(rgl)
require(ggplot2)

# %% Machine Learning Online Class - Exercise 2: Logistic Regression
# %
# %  Instructions
# %  ------------
#   % 
# %  This file contains code that helps you get started on the second part
# %  of the exercise which covers regularization with logistic regression.
# %
# %  You will need to complete the following functions in this exericse:
#   %
# %     sigmoid.m
# %     costFunction.m
# %     predict.m
# %     costFunctionReg.m
# %
# %  For this exercise, you will not need to change any code in this file,
# %  or any other files other than those mentioned above.
# %

data <- read.csv('ex2data2.txt', header=FALSE)
names(data) <- c('Test1', 'Test2', 'Outcome')
X <- data[, 1:2]
y <- data[, 3]

old.theme <- theme_set(theme_bw())
qplot(Test1, Test2, data=data, 
      colour=as.factor(Outcome), shape=as.factor(Outcome))

# %% =========== Part 1: Regularized Logistic Regression ============
# %  In this part, you are given a dataset with data points that are not
# %  linearly separable. However, you would still like to use logistic 
# %  regression to classify the data points. 
# %
# %  To do so, you introduce more features to use -- in particular, you add
# %  polynomial features to our data matrix (similar to polynomial
#                                            %  regression).
# %
# 
# % Add Polynomial Features

# % Note that mapFeature also adds a column of ones for us, so the intercept
# % term is handled
degrees <- 6
X <- mapFeature(X[, 1], X[, 2], degrees)

# Initialize fitting parameters
initial_theta <- rep(0, dim(X)[2])

# Set regularization parameter lambda to 1
lambda <- 1

# Compute and display initial cost and gradient for regularized logistic
# regression
cost <- costFunction(initial_theta, X, y, lambda)
grad <- gradFunction(initial_theta, X, y, lambda)

cat('Cost at initial theta (zeros):', cost)
cat('Gradient at initial theta (zeros):', grad)

# %% ============= Part 2: Regularization and Accuracies =============
# %  Optional Exercise:
# %  In this part, you will get to try different values of lambda and 
# %  see how regularization affects the decision coundart
# %
# %  Try the following values of lambda (0, 1, 10, 100).
# %
# %  How does the decision boundary change when you vary lambda? How does
# %  the training set accuracy vary?
# %

# Initialize fitting parameters
initial_theta <- rep(0, dim(X)[2])

# Set regularization parameter lambda to 1 
lambda <- 1

# Optimize using BFGS algorithm
system.time(
  optim.fit <- optim (par     = initial_theta, 
                      fn      = costFunction, 
                      gr      = gradFunction, 
                      method  = 'BFGS', 
                      control = list(maxit=400),
                      X, y, lambda)
)

cat('Cost at optimal theta:', optim.fit$value)
cat('Gradient at at optimal theta', optim.fit$par)

# overfitting with glm
glm.fit <- glm (y ~ X[, -1], family=binomial(link="logit"))
display(glm.fit)

# also does not seem to produce stable estimates
bayesglm.fit <- bayesglm (y ~ X[, -1], family=binomial(link="logit"),
                          prior.scale=2.5, prior.df=1)
display(bayesglm.fit)

# Plot Boundary
theta <- optim.fit$par
plotDecisionBoundary(theta, X, y, degrees)
#d <- ggplot(data.melted, aes(u, v, z = value))
#d + stat_contour()

# Compute accuracy on our training set
prob <- invlogit(X %*% theta)
pred <- ifelse(prob >= 0.5, 1, 0)
accuracy <- mean(pred == y)
cat('Train Accuracy:', accuracy)       


