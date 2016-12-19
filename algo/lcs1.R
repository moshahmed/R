# What: LCS in R
# https://en.wikipedia.org/wiki/Longest_common_substring_problem
# Translated from https://www.ics.uci.edu/~eppstein/161/960229.html
# Changes GPL(C) moshahmed@gmail.com

lcs1 <- function(X, Y) {
  S <- unlist(strsplit(X,split=""))
  T <- unlist(strsplit(Y,split=""))
  m <- nchar(X)
  n <- nchar(Y)
  if (m<1 || n < 1) return(0)
  L <- matrix( 0,nrow=m+1,ncol=n+1)
  for (i in m:1) {
    for (j in n:1){
      if (S[i] == 0 || T[j] == 0) L[i,j] = 0
      else if (S[i] == T[j]) L[i,j] = 1 + L[i+1, j+1]
      else L[i,j] = max(L[i+1, j], L[i, j+1]);
    }
  }
  # View(L)
  return(L[1,1])
}
print(lcs1("XMJYAUZ","MZJAWXU"))
