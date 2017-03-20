setwd("~/Documents/Github/Optimization/Group HW3")
load(file = 'data.rdata')
library(gurobi)
library(glmnet)

findBeta <- function(X_, y_, k) {
  
  # Number of variables
  numV <- dim(X_)[2]
  M <- 1 # default value of M
  
  # Add z's
  X_ <- cbind(X_, matrix(0,dim(X_)[1],numV))
  
  # Make constraints
  
  # Our x vector has 2*numV columns: the first numV columns represent the betas for each variable. The last numV are z variables
  
  # Constraint 1: Sum of z is less than or equal to k
  A <- matrix(0,1,2*numV) # make A matrix.
  A[,(numV+1):(2*numV)] <- 1 # last numV columns are for z variables
  
  b <- c(k)
  
  
  # Constraint 2A: -beta - Mz <= 0 
  
  A.2.1 <- matrix(0,numV,2*numV)
  A.2.1[1:numV,1:numV] <- diag(-1,numV)
  A.2.1[1:numV, (numV+1):(2*numV)] <- diag(-M, numV)
  
  b <- append(b, rep(0,numV))
  
  A <- rbind(A, A.2.1) # append rows of A matrix
  
  # Constraint 2B: beta - Mz <= 0 
  A.2.2 <- matrix(0,numV, 2*numV)
  A.2.2[1:numV, 1:numV] <- diag(1,numV)
  A.2.2[(1:numV), (numV+1):(2*numV)] <- diag(1, numV)
  A.2.2[1:numV, (numV+1):(2*numV)] <- diag(-M, numV)
  
  b <- append(b, rep(0,numV))
  
  A <- rbind(A, A.2.2)
  
  # Make dir
  dir <- rep("<=", (2*numV)+1)

  
  # Create min function
  Q <- .5 * t(X_) %*% (X_)
  c <- -1*t(t(y_) %*% X_)
  alpha <- .5 * t(y_) %*% y_
  
  # Define model
  model <- list()
  params <- list(Method=2)
  
  # Set model parameters
  model$A <- A
  model$Q <- Q
  model$obj <- c
  model$objcon <- alpha
  model$modelsense <- "min"
  model$rhs <- b
  model$sense <- dir
  model$vtype <- append(rep('C',numV),rep('B',numV))
  
  # Solve
  model.solve <- gurobi(model, params)
  model.coef <- model.solve$x[1:numV]
  
  # Check if the magnitude of any coefficient in the solution is greater than M. If such a coefficient exists, double M and resolve.
  # Repeat this until there are no coefficients whose magnitude exceeds M
  
  while (sum(abs(model.coef) >= M) != 0) {
    M <- M * 2
    model.solve <- gurobi(model, params)
    model.coef <- model.solve$x[1:numV]
  }
  
  model.solve
  
}

solution <- findBeta(X, y, 8)$x[1:64]
(solution != 0) == (beta_real != 0) # Check betas match

# Use glmnet to find 8 betas

lasso <- glmnet(X, y, alpha=1, family="gaussian", intercept=FALSE)

coef(lasso)[,5]
sum(coef(lasso)[,5] != 0)

#######################################
#[,5]?????
#######################################

#Q2.Apply both MIQP and Lasso to the given data

#use cross validation to find best lambda for lasso regression
cv.lasso <- cv.glmnet(X, y, alpha=1,standardize=FALSE,intercept=FALSE)
plot(cv.lasso, xvar = "lambda", label = TRUE)
#groph shows cutoff point as log(lambda)=-2, lambda=exp(-2). 
#The number of variables remains when lambda=exp(-2) is about 8, we can start from here.

lasso <- coef(cv.lasso, s = exp(-2))
sum(lasso != 0) #check whether the number of variables is 8 when using the cutoff lambda

#try a larger lambda to reduce the number of non-zero betas to 8
lasso <- coef(cv.lasso, s = exp(-1.8))
sum(lasso != 0)

#now we have 8 non-zero betas, let's check betas match
(lasso[2:65] != 0) == (beta_real != 0)

#Q3. Compare the prediction error for each regression
# Make predictions with MIQP
predict <- X %*% solution
actual <- X %*% beta_real
sum((predict-actual)**2) / sum(actual**2)

# Make predictions with Lasso
predict_Lasso <- X %*% lasso[2:65]
sum((predict_Lasso-actual)**2) / sum(actual**2)
