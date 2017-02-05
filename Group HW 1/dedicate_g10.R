setwd("~/Documents/Github/Optimization/Group HW 1")
library(lpSolve)

# Question 3

# Helper function to convert bonds with 0 and non-integer maturity lengths to 1 and the ceiling (i.e. round up), respectively
maturity.convert <- function(x) {
  ifelse(x==0, 1, ceiling(x))
}

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
  M.matrix <- matrix(0, nrow = num.years, ncol = num.bonds) # M matrix for whether maturity payment is made
  B.matrix <- matrix(0, nrow = num.years, ncol = num.bonds) # B matrix is for whether coupon payment is made
  
  # Create time vectors for maturity and coupon. A bond that matures before the first liability would have a maturity of 0 but
  # we would still get the face value. However, we won't get a coupon. So our M and B matrices need to capture this fact.
  # If a bond matures in 1.5 years, the cash-flow from the face value shows up in year 2 but we only receive 2 annual coupons (assume annual for this example, not the HW)
  # If a bond matures in 2 months, the cash flow from the face value shows up in year 1, but there is 0 coupon.
  
  M.face <- sapply(M, maturity.convert) # bonds with 0 maturity have value of 1; otherwise, bonds with maturities between integers are rounded up.
  C.pay <- sapply(M, floor)
  
  # Fill in B & M matrix
  for (bond_num in c(1:num.bonds)) {

    num.coupon.pmt <- C.pay[bond_num]
    
    maturity.yr <- min(num.years, M.face[bond_num])
    coupon.yrs <- ifelse(num.coupon.pmt == 0, 0 ,min(num.years,num.coupon.pmt))

    M.matrix[maturity.yr, bond_num] <- 1
    B.matrix[1:coupon.yrs, bond_num] <- ifelse(coupon.yrs != 0, 1, 0)
  }
  
  # Create A matrix
  A <- matrix(0, nrow = num.bonds + num.years, ncol = num.bonds)
  
  # Fill in A matrix columns for liability constraints. Row is year and column is bond #
  for (year in c(1:num.years)) {
    # Compute coupon and maturity payment per unit of bond
    coupon.pmt <- C * B.matrix[year,]
    maturity.pmt <- f * M.matrix[year,]
    
    # Add cash in-flow per unit of bond
    A[year,1:num.bonds] <- coupon.pmt + maturity.pmt
  }
  
  # Non-negativity 
  A[(num.years + 1):(num.years + num.bonds), 1:num.bonds] <- diag(1, num.bonds, num.bonds)
  
  # Solve
  model <- lp("min",c.model, A, dir, b, compute.sens = TRUE)
  model
}

# Test Case
p <- c(102, 99, 101, 98, 98, 104, 100, 101, 102, 94)
c <- c(5, 3.5, 5, 3.5, 4, 9, 6, 8, 9, 7)
m <- c(1,2,2,3,4,5,5,6,7,8) 
l <- c(12000, 18000, 20000, 20000, 16000, 15000, 12000, 10000)

# Run function using test case
q3.test <- dedicate(p, c, m, l)
q3.test$solution
# ------------ QUESTION 4 ---------------------------------

# Read in wall street journal prices
wsj <- read.csv('Bond Prices.csv')

# Show data summary to make sure numerics aren't factors. Chg is a factor even though it should be number
str(wsj)

# Which level is not numeric? "unch."
levels(wsj$Chg)

# Change to numeric and convert NA to 0.
wsj$Chg <- as.numeric(levels(wsj$Chg))[wsj$Chg]
wsj$Chg[is.na(wsj$Chg)] <- 0

# Change maturity to date
wsj$Maturity <- as.Date(wsj$Maturity, "%m/%d/%y")

# Preview wsj
head(wsj)

# Portfolio start date
start.date <- as.Date("1/1/2017", "%m/%d/%Y")

# Define inputs to function
price <- wsj$Asked
coupon <- wsj$Coupon * 0.5 # each coupon payment is made semi-annually while the listed coupon is the annual payout
liability <- c(9000000, 9000000, 10000000, 10000000, 6000000, 6000000, 9000000, 9000000, 10000000, 10000000, 5000000, 3000000)

# Define maturity input
maturity.dates <- wsj$Maturity # need to convert to months or years
semi.annual <- difftime(maturity.dates,start.date, units="weeks") / 26 # convert weeks to semi-annual unit
#c maturity <- floor(semi.annual) # Round down. For example, a bond that matures in 1.07 half-years (i.e. 6 months and some change) from today should have a maturity value of 1
maturity <- as.numeric(semi.annual)
  
  
q4.sol <- dedicate(price, coupon, maturity, liability)
q4.sol$solution
# Sensitivity of question 3 as a test

constraints.idx <- c(1:q3.test$const.count) # Constraint index
var.idx <- c((q3.test$const.count+1):(q3.test$const.count+q3.test$x.count)) # Variable index

constraint.sens <- data.frame(q3.test$duals[constraints.idx], # Create dataframe for constraint sensitivity
                              q3.test$duals.from[constraints.idx], 
                              q3.test$duals.to[constraints.idx])

variable.sens <- data.frame(q3.test$duals[var.idx], # Create dataframe for variable sensitivity
                            q3.test$duals.from[var.idx], 
                            q3.test$duals.to[var.idx])

coef.sens <- data.frame(q3.test$sens.coef.from, q3.test$sens.coef.to)

colnames(constraint.sens) <- c("Dual, Constraint", "Dual - From", "Dual - To") # Rename columns
colnames(variable.sens) <- c("Dual, Variable", "Dual - From", "Dual - To") # Rename columns
colnames(coef.sens) <- c("Coef Sensitivity - From", "Coef Sensitivity - To") # Rename columns

constraint.sens # Display constraint sensitivities
variable.sens # Display variable sensitivities. Not sure how to interpret these
coef.sens # Display coefficient sensitivities

# -------Sensitivity Analysis of Question 4 ----------------------------------



