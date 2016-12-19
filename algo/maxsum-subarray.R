# What: Maximal sum sub array in R
# 2016-Oct-02 
# Changes GPL(C) moshahmed@gmail.com
# see C:/doc3/algo/malgo/dynamic/msa/maxsum-subarray.c
# $Header: c:/cvs/repo/mosh/stats-r/algo/maxsum-subarray.R,v 1.5 2016-12-04 15:15:23 a Exp $

maximal_sum_subarray <- function(A) {
  N=length(A)
  b=-1
  e=-1
  max_sum=-Inf
  for(j in 0:(N-1)) { # for(j=0;j<N;j++)
    sum=0
    for(i in j:(N-1)) { # for(i=j;i<N;i++)
      sum = sum + A[i+1]
      if(sum>max_sum){
        max_sum=sum
        b=j
        e=i
      }
    }
  }
  cat(sprintf("maximal_sum_subarray of A[%d:%d] is (",b,e),A[(b+1):(e+1)],")")
  cat(" max_sum=",max_sum,"\n")
}

A <- c(13,-3,-25,20,-3,-16,-23,18,20,-7,12,-5,-22,15,-4,7)
cat("A=",A,"\n")
maximal_sum_subarray(A)
