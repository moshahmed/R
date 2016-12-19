# Homework. Fill a matrix diagonally in 2D enum.
# Changes GPL(C) moshahmed@gmail.com

#          x
#  1   2   6
#  3   5   7
#y 4   8   11
#  9   10  12

# > source('~/stats-r/algo/spiral3.R')
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    4   10   11
# [2,]    2    5    9   12   19
# [3,]    6    8   13   18   20
# [4,]    7   14   17   21   24
# [5,]   15   16   22   23   25

spiral3 <- function(A) {
  outside <- function(A,i,j){
    dij <- c(i,j)
    if (any(dij < c(1,1))) return(TRUE)
    if (any(dij > dim(A))) return(TRUE)
    return (FALSE)
  }
  turn <- function(A,i,j){
    if (outside(A,i,j)) return (1)
    if( A[i,j] != 0 ) return(1)
    return (0)
  }
  A[] <- 0
  ar = ac = 1
  k=1; oldk=0;
  s=1; sm = length(A)*2;
  while(k>oldk) {
    oldk=k
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ar=ar+1; ac=ac-1;} # Down-Left
    ac <- ac+1; # at y, backup
    while(outside(A,ar,ac) && s<sm){ ac=ac+1; ar=ar-1; s=s+1 }

    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1; ar=ar-1; ac=ac+1} # Up-Right
    ar=ar+1; # at x, backdown
    while(outside(A,ar,ac) && s<sm){ ac=ac-1; ar=ar+1; s=s+1 }
  }
  return(A)
}

a <- 5
b <- 5
A <- matrix(1,a,b)
B <- spiral3(A)
print(B)


