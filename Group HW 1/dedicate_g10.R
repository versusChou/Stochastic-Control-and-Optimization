setwd("~/Dropbox/MSBA/Spring 2016/Stochastic Control/Group Proj/Group Proj 1")
library(lpSolve)

# Question 3

dedicate <- function(P,C,M,L) {
  
  # Create helper variables
  
  f <- 100 #  face value
  num.bonds <- length(P) # number of bonds
  num.years <- length(L)
  
  # c.model is the vector of coefficients for the objective function
  c.model <- P 
  
  # Equality constraints
  b <- L # liabilities
  b <- append(b, rep(0,num.bonds)) # non-negative
  
  # Direction
  dir <- c(rep(">=",num.years + num.bonds))
  
  # Helper matrices
  M <- matrix(0, nrow = num.years, ncol = num.bonds) # M matrix for whether maturity payment is made
  B <- matrix(0, nrow=num.years, ncol = num.bonds) # B matrix is for whether coupon payment is made
  
  # Fill in B & M matrix
  for (bond_num in c(1:num.bonds)) {
    maturity.yr <- m[bond_num]
    coupon.yrs <- c(1:maturity.yr)
    
    M[maturity.yr, bond_num] <- 1
    B[coupon.yrs, bond_num] <- 1
  }
  
  # Create A matrix
  A <- matrix(0, nrow = num.bonds + num.years, ncol = num.bonds)
  
  # Fill in A matrix columns for liability constraints. Row is year and column is bond #
  for (year in c(1:num.years)) {
    # Compute coupon and maturity payment per unit of bond
    coupon.pmt <- c * B[year,]
    maturity.pmt <- f * M[year,]
    
    # Add cash in-flow per unit of bond
    A[year,1:num.bonds] <- coupon.pmt + maturity.pmt
  }
  
  # Non-negativity 
  A[(num.years + 1):(num.years + num.bonds), 1:num.bonds] <- diag(1, num.bonds, num.bonds)
  
  # Solve
  model <- lp("min",c.model, A, dir, b)
  model$solution
}

p <- c(102, 99, 101, 98, 98, 104, 100, 101, 102, 94)
c <- c(5, 3.5, 5, 3.5, 4, 9, 6, 8, 9, 7)
m <- c(1,2,2,3,4,5,5,6,7,8) 
l <- c(12000, 18000, 20000, 20000, 16000, 15000, 12000, 10000)

# Run function using test case
dedicate(p, c, m, l)


