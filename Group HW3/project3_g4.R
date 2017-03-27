# setwd("~/Documents/Github/Optimization/Group HW3")
load(file = 'data.rdata')
library(gurobi)
library(glmnet)

#create function for MIQP
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

# Store betas from MIQP
solution <- findBeta(X, y, 8)$x[1:dim(X)[2]]
# Check betas match. Sum should be 64
sum((solution != 0) == (beta_real != 0))

# Apply Lasso to the given data 
lasso <- glmnet(X, y, alpha=1,intercept=FALSE, family='gaussian')

# Build matrix that stores number of coefficients per lambda. The next loop will write the number of coefficients per lambda
mat.coef <- matrix(0,length(lasso$lambda),3) # store number of coefficients
actual <- X %*% beta_real # actual values

# Generate number of coefficients and prediction error for each lambda
for (model in 1:length(lasso$lambda)){
  num.coef <- coef(lasso, s = lasso$lambda[model])
  predict_Lasso <- X %*% num.coef[2:65]
  error<-sum((predict_Lasso-actual)**2) / sum(actual**2)
  mat.coef[model,] <- c(lasso$lambda[model],sum(num.coef != 0),error)
}

# Store prediction error from MIQP
predict_MIQP <- X %*% solution
error_MIQP <- sum((predict_MIQP - actual)**2) / sum(actual**2)

### Questions 2 and 3 Plots

# Q2
plot(mat.coef[,1], mat.coef[,2],xlab="lambda", ylab="coefficients", main="Lasso: Coefficients vs. Lamda")
# Q3
plot(mat.coef[,1], mat.coef[,3], xlab="lambda", ylab="errors", main="Lasso: Lambda vs. error") 
abline(h = error_MIQP,col = "red")
legend("topleft","MIQP error",lty=1,cex=1,bty="n",col="red")

