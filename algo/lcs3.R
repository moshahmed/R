# What: LCS in R
# translated from http://www.geeksforgeeks.org/printing-longest-common-subsequence/
# Changes GPL(C) moshahmed@gmail.com

# getwd()
# setwd("C:/mosh/stats-R/algo/")
# source("lcs3.R")

printf <- function(...) invisible(print(sprintf(...))) 

lcs3 <- function(Xm, Yn) {
  X <- unlist(strsplit(Xm,split=""))
  Y <- unlist(strsplit(Yn,split=""))
  m <- nchar(Xm)
  n <- nchar(Yn)
  if (m<1 || n < 1) return(0)
  L <- matrix( 0,nrow=m+2,ncol=n+2)
  for (i in 1:m+1) {
    for (j in 1:n+1){
       if (i == 1 || j == 1)
         L[i,j] = 0
       else if (X[i-1] == Y[j-1])
         L[i,j] = L[i-1,j-1] + 1
       else
         L[i,j] = max(L[i-1,j], L[i,j-1])
    }
  }
  View(L)
  i <- m+1
  j <- n+1;
  lcs <- c()
  while( i>1 && j > 1) {
      if (X[i-1] == Y[j-1]) {
          lcs <- c( X[i-1], lcs)
          i =i-1
          j =j-1
      }
      else if (L[i-1,j] > L[i,j-1]) i =i-1
      else j =j-1
  }
  cat('lcs of [',X,'] and [',Y,'] is [',lcs,'] len=',L[m+1,n+1],"\n")
  return(L[m+1,n+1])
}
lcs3("AGGTAB","GXTXAYB")
lcs3("abcd", "acdb")
