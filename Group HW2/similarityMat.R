setwd("~/Documents/Github/Optimization/Group HW2")

source('readData.R')

# The similarity matrix is the negative of the euclidean distance between 2 stocks.
# The components of each stock vector is the price on a given date.

# Build function similarityMat.
similarityMat <- function(price_matrix, shares_matrix, uniqueTickers, uniqueDates) {
  N <- length(uniqueTickers)
  
  rhoMat <- matrix(0,N,N)
  for (i in 1:N) {
    for (j in 1:N) {
      rhoMat[i,j] <- -dist(rbind(price_matrix[,i], price_matrix[,j]))
    }
  }
  
  return(rhoMat)
}

# Compute weights for Q5


# Calculate portfolio return
portfolioReturn <- function(weights) {
  base.value <- 1000000
  allocate.funds <-  weights * base.value # total amount allowed to spend for each stock
  shares.portfolio <- allocate.funds / tail(priceMat,1) # number of shares purchased
  
  # Calculate portfolio value at the end of every month, including base month of 12/2012
  portfolio.value <- c(base.value)
  for (i in 1:12) {
    result <- sum(monthlyPriceMat[i,] * shares.portfolio) # portfolio value at the end of month
    portfolio.value <- c(portfolio.value, result)
  }
  # Calculate monthly returns of portfolio for 2013
  portfolio.return <- c()
  
  for (month in 2:length(portfolio.value)) {
    return <- (portfolio.value[month] / portfolio.value[month - 1]) - 1
    portfolio.return <- c(portfolio.return, return)
  }
  
  return(portfolio.return)
}




