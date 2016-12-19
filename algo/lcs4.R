# What: LCS in R
# Changes GPL(C) moshahmed@gmail.com
# See c:/doc3/algo/malgo/dynamic/lcs/lcs.htm
# See lcs3.R 2016-10-15

lcs4p <- function(B, X, i, j) {
  # cat("lcs4p",i,j,"\n")
  if ( i==0 || j==0 ) return()
  if (B[i+1, j+1] == '/') {
    lcs4p(B, X, i-1, j-1)
    print (X[i])
  } else if (B[i+1, j+1] == '^') {
    lcs4p(B, X, i-1, j)
  } else { # '<'
    lcs4p(B, X, i, j-1)
  }
}

lcs4 <- function(Xm, Yn) {
  X <- unlist(strsplit(Xm,split=""))
  Y <- unlist(strsplit(Yn,split=""))
  m <- nchar(Xm)
  n <- nchar(Yn)
  if (m<1 || n < 1) return(0)

  L <- matrix(0, nrow=m+2, ncol=n+2)
  B <- L

  for (i in 1:m+1) {
    for (j in 1:n+1) {
      if (i == 1 || j == 1) {
         L[i,j] = 0
      } else if (X[i-1] == Y[j-1]) {
        L[i, j] = L[i-1, j-1] + 1
        B[i, j] = '/'
      } else if (L[i-1,j] >= L[i,j-1]) {
          L[i, j] = L[i-1, j]
          B[i, j] = '^'
      } else {
          L[i, j] = L[i,j-1]
          B[i, j] = '<'
      }
    }
  }
  lcs4p(B, X, m, n)
  return(list("L"=L,"B"=B))
}

Xm <- "ABABAD"
Yn <- "ACAADD"
ans <- lcs4(Xm,Yn)


