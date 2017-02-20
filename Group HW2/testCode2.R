setwd("~/Documents/Github/Optimization/Group HW2")

# Load files
source('readData.R')
q=25
source('similarityMat.R')
source('constructFund.R')
rho = similarityMat(priceMat, sharesMat, unique_tickers,unique_dates)
weights = constructFund(corrMat, q, priceMat, sharesMat, unique_tickers, unique_dates)

# --------------------- Calculate NASDAQ ------------
monthlyPriceMat
price.2012 <- tail(priceMat,1)
shares.2012 <- tail(sharesMat,1)

market.cap.individual <- price.2012 * shares.2012
total.market.cap <- sum(market.cap)

weights.nasdaq <- market.cap.individual / total.market.cap

initial.fund <- 1000000
invest.fund <- initial.fund * weights.nasdaq
num.shares.fund <- invest.fund / price.2012 # number of shares of every stock in portfolio. Stays same

nasdaq.value <- c(1000000)
nasdaq.value <- append(nasdaq.value, monthlyPriceMat %*% t(num.shares.fund))

nasdaq.returns <- c()
for (i in 2:13) {
  return.amount <- (nasdaq.value[i] / nasdaq.value[i-1]) - 1
  nasdaq.returns <- append(nasdaq.returns, return.amount)
}

plot(nasdaq.returns, portfolioReturn(weights))


