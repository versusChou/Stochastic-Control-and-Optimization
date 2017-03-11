setwd("~/Documents/Github/Optimization/Group HW3")
load(file = 'data.rdata')
library(gurobi)
library(glmnet)

findBeta <- function(X_, y_, numZ) {
  
  # Number of variables
  k <- dim(X_)[2]
  M <- 1000
  
  # Add z's
  X_ <- cbind(X_, matrix(0,dim(X_)[1],k))
  
  # Make constraints
  
  # Our x vector has 2*k columns: the first k columns represent the betas for each variable. The last k are z variables
  
  # Constraint 1: Sum of z is less than or equal to k
  A <- matrix(0,1,2*k) # make A matrix.
  A[,(k+1):(2*k)] <- 1 # last k columns are for z variables
  
  b <- c(numZ)
  
  
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

  
  # Create min function
  Q <- .5 * t(X_) %*% (X_)
  c <- -.5 * t(X_) %*% y_
  alpha <- t(y_) %*% y_
  
  model <- list()
  params <- list(Method=2)
  
  model$A <- A
  model$Q <- Q
  model$obj <- c
  model$objcon <- alpha
  model$modelsense <- "min"
  model$rhs <- b
  model$sense <- dir
  model$vtype <- append(rep('C',k),rep('B',k))
  
  gurobi(model, params)
  # cbind(A, dir, b)
}

# Check if our solution matches actual solution
findBeta(X[1:dim(X)[1],1:64], y, 8)$x[65:128] == beta_real
