setwd("~/Documents/Github/Optimization/Group HW3")
load(file = 'data.rdata')
library(gurobi)
library(glmnet)

findBeta <- function(X_, y_) {
  # Number of variables
  k <- dim(X_)[2]
  M <- 1000
  
  # Make constraints
  
  # Our x vector has 2*k columns: the first k columns represent the betas for each variable. The last k are z variables
  
  # Constraint 1: Sum of z is less than or equal to k
  A <- matrix(0,1,2*k) # make A matrix.
  A[,(k+1):(2*k)] <- 1 # last k columns are for z variables
  
  b <- c(k)
  
  
  # Constraint 2A: -beta - Mz <= 0 
  
  A.2.1 <- matrix(0,k,2*k)
  A.2.1[1:k,1:k] <- diag(-1,k)
  A.2.1[1:k, (k+1):(2*k)] <- diag(-M, k)
  
  b <- append(b, rep(0,k))
  
  A <- rbind(A, A.2.1) # append rows of A matrix
  
  # Constraint 2B: beta - Mz <= 0 
  A.2.2 <- matrix(0,k, 2*k)
  A.2.2[1:k, 1:k] <- diag(1,k)
  A.2.2[(1:k), (k+1):(2*k)] <- diag(1, k)
  A.2.2[1:k, (k+1):(2*k)] <- diag(-M, k)
  
  b <- append(b, rep(0,k))
  
  A <- rbind(A, A.2.2)
  
  # Make dir
  dir <- rep("<=", (2*k)+1)
  
  test.X <- X[1:10,1:3] 
  test.y <- y[1:10]
  
  Q <- t(X_) %*% X_
  c <- -t(X_) %*% y_
  
  
}

findBeta(X[1:3, 1:3], y[1:3])


