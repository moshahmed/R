# What: Dynamic programming, Optimal Matrix Chain Multiplication in R
# Changes GPL(C) moshahmed@gmail.com
# 2016-10-07
# From https://en.wikipedia.org/wiki/Matrix_chain_multiplication
# see https://www.hackerrank.com/topics/dynamic-programming

dims <- c(1,2,3,4,3) # global min=30.
dims <- c(40, 20, 30, 10, 30) # ans=26000
n <- length(dims) - 1
m <- matrix(0, nrow=n, ncol=n )
s <- m
for (len in 2:(n)) {
  for (i in 1:(n-len+1) ) {
    j = i + len - 1
    m[i,j] = Inf
    for (k in i:(j-1)) {
      cost = m[i, k] + m[k+1, j] + dims[i-1 +1]*dims[k +1]*dims[j +1];
      if ( cost < m[i, j] ) {
        m[ i, j ] = cost
        s[ i, j ] = k
      }
    }
  }
}

