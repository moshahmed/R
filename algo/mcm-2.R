# What: Dynamic programming, Optimal Matrix Chain Multiplication in R
# Changes GPL(C) moshahmed@gmail.com
# From https://en.wikipedia.org/wiki/Matrix_chain_multiplication
# See https://www.hackerrank.com/topics/dynamic-programming
# 2016-10-07

dims <- sample(1:5, 5)
n <- length(dims) - 1
m <- matrix(0, nrow=n, ncol=n )
s <- m
for (len in 2:(n)) {
  if (len > n) break
  for (i in 1:(n-len+1) ) {
    if (i > n-len+1) break
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
print(dims)
print(m)
print(s)
# View(m); View(s)
get_optimal <- function(i,j) {
  cat(sprintf("get_optimal(i=%d,j=%d)\n",i,j))
  if (i==j) { # One matrix
    return(sprintf("A%d",i))
  }
  if ((j-i) <= 1) { # Two matrix
    return(sprintf("(A%d,A%d)", i, j))
  }
  a <- get_optimal(i, s[i,j]     )
  b <- get_optimal(   s[i,j]+1, j)
  c <- sprintf("(%s,%s)",a,b)
  return(c)
}
ans <- get_optimal(1,n)
print(ans)
