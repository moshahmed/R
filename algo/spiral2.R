# What: Fill a matrix with snaking numbers in R
# increasing seq going up and down to right.
# Changes GPL(C) moshahmed@gmail.com

#  1   8   9
#  2   7   10
#  3   6   11
#  4   5   12

spiral2 <- function(A) {
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

    if(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1; ac=ac+1} # Right
    ac=ac-1; ar=ar+1;

    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ar=ar+1} # Down
    ar=ar-1; ac=ac+1;

    if(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1; ac=ac+1}  # Right
    ac=ac-1; ar=ar-1;

    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ar=ar-1} # Up
    ar=ar+1; ac=ac+1;
  }
  return(A)
}

a <- 4
b <- 5
A <- matrix(1,a,b)
B <- spiral2(A)
print(B)


