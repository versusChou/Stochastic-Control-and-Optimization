setwd("~/Documents/Github/Optimization/Group HW2")
library(lpSolve)
# Load data
{
  # Read in clean data from cleaning file
  source('readData.R')
  
  # Daily Price Matrix. Column 40 is NA for first few weeks
  # head(priceMat)
  
  # Shares Matrix
  # head(sharesMat)
  
  # Monthly Price Matrix
  # head(monthlyPriceMat) 
  
  # Unique tickers & dates
  # unique_dates
  # unique_tickers
}

# ------ Question 1 - Calculate daily returns for each stock
{
  
  num.price.rows <- dim(priceMat)[1] # store number of rows in price matrix
  
  dailyReturnMat <- priceMat[2:num.price.rows, ] # use price matrix as template for daily return matrix
  
  for (i in 2:num.price.rows) {
    return.value <- (priceMat[i,] / priceMat[i-1,]) - 1
    dailyReturnMat[i-1,] <- return.value
  }
  
  # head(dailyReturnMat) # matrix of daily returns
}

# ------ Question 2 - Correlation matrix for returns of 100 stocks
{
  corrMat <- cor(dailyReturnMat, use = "complete.obs")
  
}

# ------ Question 3 - Code integer program as a function to determine weights for our index fund
# find.sim.stocks finds the j stocks that are most similar to the index
find.sim.stocks <- function(rho, q, uniqueTickers) {
{
  
  N <- length(uniqueTickers)


  # Create c vector. First 10,000 items are x_1,1, x_1,2... x_100,100. Last 100 items are y_1 to y_100
  c.x <- c() # create c vector for x_ij
  
  for (j in 1:N) { # Loop through each column and append column to c vector
    c.x <- append(c.x, rho[,j])
  }
  
  c.y <- rep(0,N) # create c vector for y_j
  
  c <- append(c.x, c.y) # final c vector

  # Create A matrix
  n.col <- length(c)
  
  num.x <- length(c.x)
  num.y <- length(c.y)
  
  A <- matrix(0,nrow = 0, ncol = n.col)
  
  # # Create b vector
  b <- c()
  # 
  # Create dir vector
  dir <- c()
  
  # Constraint 1: Sum of y_j = q
  A.1 <- c(rep(0, num.x), rep(1, num.y))
  A <- rbind(A, A.1)

  b <- append(b,q)
  
  dir <- append(dir, "=")
  
  # Constraint 2 - For each stock i, the sum of x_i,1 to x_i,j must equal 1. 
  # In other words, every stock must only have 1 representative stock.
  # Our x-vector is [x_(1,1), x_(1,2), ..., x_(100,1),...x_(100,100)]

  for (j in (1:N)) { # Add constraint 2 to A matrix
    A.2 <- rep(0, num.x + num.y)
    b.2 <- 1
  
    start.ix <- N*(j-1) + 1
    end.ix <- N*j
    constraint.2.range <- c(start.ix:end.ix)

    A.2[constraint.2.range] <- 1

    A <- rbind(A, A.2)
    b <- append(b, b.2)
    dir <- append(dir, "=")
  }
  
  # Constraint 3 - Stock i can only be represented by stock j if stock i is in the fund
  {
    # For x constraints, create identity matrix.

    A.3.x <- diag(1,num.x) # matrix of x values for constraint 3
    dir <- append(dir, rep("<=",num.x))

    # For y constraints, create columns of -1 for every group of 100 rows

    A.3.y <- matrix(0,0,num.y)

    for (j in 1:N) {

      A.3.y.temp <- matrix(0,N, N)
      A.3.y.temp[1:N,] <- diag(-1,N)
  
      A.3.y <- rbind(A.3.y, A.3.y.temp)
    }

    # To create final A matrix for constraint 3, cbind x and y matrices for constraint 3
    A.3 <- cbind(A.3.x, A.3.y)

    # Rbind constraint 3's A-matrix to the overall A matrix
    A <- rbind(A, A.3)

    # Add constraint 3 to b vector
    b.3 <- rep(0, num.x)
    b <- append(b, b.3)

    # Solve
    q3.sol <- lp("max",c,A,dir,b, all.bin = TRUE)
    q3.sol$solution
  }
  }  
}

# Test find.sim.stocks
find.sim.stocks(corrMat[1:3,1:3], 2, unique_tickers[1:3])


# Function calc.weights to calculate weights
constructFund <- function(rho, q, price, numShares, uniqueTickers, uniqueDates) {
  
  N.days <- length(uniqueDates)
  final.date <- rownames(numShares)[N.days]
  num.stocks <- length(uniqueTickers)
  V <- numShares[which(rownames(numShares) == final.date),]
  
  # Create x matrix (exclude y's from solution)
  X.mat <- find.sim.stocks(rho, q, uniqueTickers)[1:(num.stocks*num.stocks)]
  X.mat <- t(matrix(X.mat, num.stocks, num.stocks))
  
  # Calculate weights for each stock j
  
  W <- c()
  for (i in 1:num.stocks) {
    weight <- V %*% X.mat[,i]
    W <- append(W,weight)
  }
  totalWeight <- sum(W)
  
  W <- W / totalWeight
  W
}

test.code <- function(n, q) {
  constructFund(corrMat[1:n,1:n], q, priceMat[,1:n], sharesMat[,1:n], unique_tickers[1:n], unique_dates)
}



