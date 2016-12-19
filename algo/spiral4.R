# Homework. Fill a matrix with snaking numbers,
# increasing seq going left-right and up-down
# Changes GPL(C) moshahmed@gmail.com

#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]   10    9    8    7    6
# [3,]   11   12   13   14   15
# [4,]   20   19   18   17   16

spiral4 <- function(A) {
  turn <- function(A,i,j){
    dij <- c(i,j)
    if (any(dij < c(1,1))) return(1)
    if (any(dij > dim(A))) return(1)
    if( A[i,j] != 0 ) return(1)
    return (0)
  }
  A[] <- 0
  ar = ac = 1
  k=1; oldk=0
  while(k>oldk) {
    oldk=k
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1; ac=ac+1} # Right
    ac=ac-1; ar=ar+1;

    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ac=ac-1} # Left
    ar=ar+1; ac=ac+1;
  }
  return(A)
}

a <- 4
b <- 5
A <- matrix(1,a,b)
B <- spiral4(A)
print(B)


