setwd("~/Documents/Github/Optimization/Group HW2")

source('readData.R')


monthlyPriceMat[1:10,1:10]
priceMat
length(unique_mtickers)
dim(monthlyPriceMat)

# Similarity  = cosine similarity

cosine.sim <- function(vector1, vector2) {
  numerator <- vector1 %*% vector2
  V1.d <- sqrt(sum(vector1**2))
  V2.d <- sqrt(sum(vector2**2))
  
  denom <- V1.d * V2.d
  return(numerator / denom)
}


for (i in 1:100) {
  for (j in 1:100) {
    
  }
}

