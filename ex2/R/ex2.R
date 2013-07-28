source('Rstart.R')
setwd('~/Dropbox/Learning/Stanford/ML/ex2/R/')

require(arm)
require(glmnet)
require(rgl)
require(ggplot2)

# %% Machine Learning Online Class - Exercise 2: Logistic Regression
# %
# %  Instructions
# %  ------------
#   % 
# %  This file contains code that helps you get started on the logistic
# %  regression exercise. You will need to complete the following functions 
# %  in this exericse:
#   %
# %     sigmoid.m
# %     costFunction.m
# %     predict.m
# %     costFunctionReg.m
# %
# %  For this exercise, you will not need to change any code in this file,
# %  or any other files other than those mentioned above.
# %
# set up the disign matrix
  
## Load and Plot Data
# The first two columns contains the exam scores and the third column
# contains the label.
old.theme <- theme_set(theme_bw())
data <- read.csv('ex2data1.txt', header=FALSE)
names(data) <- c('exam1', 'exam2', 'y')
qplot(exam1, exam2, colour=as.factor(y), data=data) 

## ============ Part 2: Compute Cost and Gradient ============
X <- data[, 1:2]
y <- data[, 3]

m <- dim(X)[1]
n <- dim(X)[2]

# add intercept term to X
X <- cbind(rep(1, m), X)

# Initialize fitting parameters
initial_theta <- rep(0, n + 1)

# Compute and display initial cost and gradient
cost <- costFunction(initial_theta, as.matrix(X), y)
grad <- gradFunction(initial_theta, as.matrix(X), y)

cat('Cost at initial theta (zeros):', cost)
cat('Gradient at initial theta (zeros):', grad)

## ============= Part 3: Optimizing using optim()  =============
# Method "BFGS" (1970) is a quasi-Newton method (also known as a variable 
# metric algorithm)
# "Nelder-Mead" (1965) uses only function values and is robust but relatively 
# slow. It will work reasonably well for non-differentiable functions
system.time(
optim.fit <- optim(par=initial_theta, 
                   fn=costFunction, 
                   gr=gradFunction, 
                   method='BFGS', 
                   control=list(maxit=400),
                   as.matrix(X), y)
)

cat('Cost at theta found by optim():', optim.fit$value)
cat('Theta:', optim.fit$par);

# compare to GLM and bayesglm()
glm.fit <- glm (y ~ exam1 + exam2, family=binomial(link="logit"), data=X)
display(glm.fit)

bayesglm.fit <- bayesglm (y ~ exam1 + exam2, family=binomial(link="logit"), 
                          prior.scale=2.5, prior.df=1, data=X)
display(bayesglm.fit)

#glmnet.fit <- glmnet(X[, -1], y, family=binomial(link="logit"))

# Plot Boundary
plotDecisionBoundary(theta = optim.fit$par, X, y)

# %% ============== Part 4: Predict and Accuracies ==============
#   %  After learning the parameters, you'll like to use it to predict the 
# outcome
# %  on unseen data. In this part, you will use the logistic regression model
# %  to predict the probability that a student with score 45 on exam 1 and 
# %  score 85 on exam 2 will be admitted.
# %
# %  Furthermore, you will compute the training and test set accuracies of 
# %  our model.
# %
# %  Your task is to complete the code in predict.m
# 
# %  Predict probability for a student with score 45 on exam 1 
# %  and score 85 on exam 2 

prob <- round(invlogit(c(1, 45, 85) %*% theta), 2)
cat('For a student with scores 45 and 85, we predict an admission prob', prob)

# Compute accuracy on our training set
X <- as.matrix(X)
prob <- invlogit(X %*% theta)
# Calculate accuracy at the 0.5 cutoff
p <- ifelse(prob > 0.5, 1, 0)
cat('Train Accuracy: ', mean(p==y))






