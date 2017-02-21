# setwd("~/Documents/Github/Optimization/Group HW2")

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


