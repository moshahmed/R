# What: Misc homework to fill matrices in R
# Changes GPL(C) moshahmed@gmail.com

# Q1. Fill a matrix with spiraling numbers:
# e.g. spiral( matrix(0, 3, 4) )
#    -------------->
#    1   2   3   4  |
# -> 10 11  12   5  v
#  ^ 9   8   7   6
#  | <------------

# > source("spiral.R")
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,]    1    2    3    4    5    6    7
# [2,]   24   25   26   27   28   29    8
# [3,]   23   40   41   42   43   30    9
# [4,]   22   39   48   49   44   31   10
# [5,]   21   38   47   46   45   32   11
# [6,]   20   37   36   35   34   33   12
# [7,]   19   18   17   16   15   14   13

spiral <- function(A) {
  turn <- function(A,i,j){
    dr <- dim(A)[1]; dc <- dim(A)[2]
    if (i < 1 || i >dr ) return(1)
    if (j < 1 || j >dc ) return(1)
    if( A[i,j] != 0 ) return(1)
    return (0)
  }
  dr <- dim(A)[1]; dc <- dim(A)[2]
  A = matrix(0, nrow=dr, ncol=dc)
  k=1; oldk=0
  ar=1;  ac=1
  while(k>oldk) {
    oldk=k
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1; ac=ac+1}
    ac=ac-1; ar=ar+1;
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ar=ar+1}
    ar=ar-1; ac=ac-1;
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ac=ac-1}
    ac=ac+1; ar=ar-1;
    while(!turn(A,ar,ac)){A[ar,ac]=k; k=k+1;ar=ar-1}
    ar=ar+1; ac=ac+1;
  }
  return(A)
}
print(spiral( matrix(1,7,7)))


