### Question 1

Assume that bond purchases are made at the beginning of time 1

Let *p* be the vector of bond prices
$$p = \\begin{bmatrix} 102 & 99 & 101 & 98 & 98 & 104 & 100 & 101 & 102 & 94 \\end{bmatrix}$$

Let *c* be the vector of bond coupon payments

$$c = \\begin{bmatrix} 5 & 3.5 & 5 & 3.5 & 4 & 9 & 6 & 8 & 9 & 7 \\end{bmatrix}$$

*u* is the vector containing maturity for the N bonds
$$u = \\begin{bmatrix} 100 &... &100 \\end{bmatrix}$$

**M** is the matrix of size *t* × *n* where *t* is the number of years
and *n* is the bond number and where *M*<sub>*t*, *n*</sub> is whether
bond *n* has reached maturity at time *t*. Below: the 1st, 2nd, 3rd and
8th rows are shown, while columns 1-4 and 8-10 are shown.

$$ \\mathbf{M} = \\begin{bmatrix} 1 & 0 & 0 & 0 & ... & 0 & 0 & 0 \\\\ 0 & 1 & 1 & 0 & ... & 0 &  0 & 0 \\\\0 & 0 & 0 & 1 & ... & 0 &  0 & 0 \\\\... \\\\ 0 & 0 & 0 & 0 & ... & 0 & 0 & 1 \\end{bmatrix}$$

**B** is the *t* × *n* matrix indicating whether there's a coupon
payment in year *t* for bond *n*

$$ \\mathbf{B} = \\begin{bmatrix} 1 & 1 & 1 & 1 & ... & 1 & 1 & 1 \\\\ 0 & 1 & 1 & 1 & ... & 0 &  0 & 0 \\\\ 0 & 0 & 0 & 1 & ... & 0 &  0 & 0 \\\\... \\\\ 0 & 0 & 0 & 0 & ... & 0 & 0 & 1 \\end{bmatrix}$$

Thus, the following can be expressed:

-   $\\sum\_{i=1}^{n}b\_{t,i}c\_{i}$ is the cash inflow from coupon
    payments from bonds 1 to *n* at time *t*
-   $\\sum\_{i=1}^{n}m\_{t,i}u\_{i}$ is the cash inflow from maturity
    from bonds 1 to *n* at time *t*
-   $\\sum\_{i=1}^{n}z^{i}$ is the total excess funds from time 1 to *n*

**Decision Variables**

-   *x*<sub>1</sub> to *x*<sub>10</sub> are the number of bonds of each
    type from 1 to 10 that are purchased

**Objective**

-   Minimize $\\sum\_{i=1}^{10}p\_{i}x\_{i}$ where *p*<sub>*i*</sub> is
    the price of bond *i*

**Constraints**

-   $\\sum\_{i=1}^{10}(b\_{i,t}c\_{i} + m\_{i,t}u\_{i}) \\times x\_{i} \\geq l\_{t}$
    where *t* is year

-   *x*<sub>*i*</sub> ≥ 0

### Question 2

    p <- c(102, 99, 101, 98, 98, 104, 100, 101, 102, 94) # prices of bonds 1-10
    c <- c(5, 3.5, 5, 3.5, 4, 9, 6, 8, 9, 7) # coupon payments of bonds 1-10
    u <- rep(100, 10) # maturity payment of bonds 1-10
    m <- c(1,2,2,3,4,5,5,6,7,8) # years to maturity for bonds 1-10


    # x1 to x10 are columns 1-10
    c.model <- p

    # Equality constraints
    b <- c(12000, 18000, 20000, 20000, 16000, 15000, 12000, 10000) # liabilities
    b <- append(b, rep(0,10)) # non-negative

    # Direction
    dir <- c(rep(">=",8),rep(">=",10))

    # Helper matrices

    M <- matrix(0, nrow = 8, ncol = 10) # M matrix for whether maturity payment is made
    B <- matrix(0, nrow=8, ncol = 10) # B matrix is for whether coupon payment is made

    # Fill in B & M matrix
    for (bond_num in c(1:10)) {
      maturity.yr <- m[bond_num]
      coupon.yrs <- c(1:maturity.yr)
      
      M[maturity.yr, bond_num] <- 1
      B[coupon.yrs, bond_num] <- 1
    }

    # Create A matrix
    A <- matrix(0, nrow = 18, ncol = 10)

    # Fill in A matrix columns for liability constraints. Row is year and column is bond #
    for (year in c(1:8)) {
      # Compute coupon and maturity payment per unit of bond
      coupon.pmt <- c * B[year,]
      maturity.pmt <- u * M[year,]
      
      # Add cash in-flow per unit of bond
      A[year,1:10] <- coupon.pmt + maturity.pmt
    }


    # Non-negativity 
    A[9:18, 1:10] <- diag(1,10,10)

    # Solve
    q1 <- lp("min",c.model, A, dir, b)
    q1$solution

    ##  [1]  62.13613   0.00000 125.24293 151.50508 156.80776 123.08007   0.00000
    ##  [8] 124.15727 104.08986  93.45794
