# What: Mergesort in R
# Changes GPL(C) moshahmed@gmail.com
# From http://stackoverflow.com/questions/26080716/merge-sort-in-r
# 2016-09-22

Merge <- function(AA, left, mid, right, indent){
  if( (right-left) > 1 ){
    cat(sprintf("%s Merge input=[%s] and [%s] \n",
      indent,
      paste(AA[left:mid],collapse=","),
      paste(AA[(mid+1):right],collapse=",")
      ))
  }
  n1 <- mid - left + 1
  n2 <- right - mid
  LESSER <- numeric(n1+1)
  GREATERS <- numeric(n2+1)
  for(i in 1:n1){
    LESSER[i] <- AA[left+i-1]
  }
  for(j in 1:n2){
    GREATERS[j] <- AA[mid+j]
  }

  LESSER[n1+1] <- Inf
  GREATERS[n2+1] <- Inf
  i=1
  j=1
  for(k in left:right){
    if(LESSER[i] <= GREATERS[j]){
      AA[k] <- LESSER[i]
      i <- i +1
    }else{
      AA[k] <- GREATERS[j]
      j <- j+1
    }
  }
  if ( (right-left)>1 ){
    cat(sprintf("%s Merge output=[%s] \n",
      indent, paste(AA[left:right],collapse=",")))
  }
  AA
}

MergeSort <- function(BB, left, right, level=1) {
  if(left >= right) return(BB)
  mid <- floor((left+right)/2)

  indent <- paste(rep(">", level),collapse="")
  cat(sprintf("%s MergeSort(BB[%d:%d]) input=[%s]\n",
    indent,left,right, paste(BB[left:right],collapse=",")))

  BB <- MergeSort(BB, left, mid, level+1)
  BB <- MergeSort(BB, mid+1, right, level+1)
  BB = Merge(BB, left, mid, right, indent)

  Mx = paste(BB[left:right],collapse=",")
  cat(sprintf("%s MergeSort(BB[%d:%d]) output=[%s]\n", indent,
    left, right, Mx ))
  return(BB)
}


x <- c(18, 16, 8, 7, 6, 3, 11, 9, 15, 1)
#x <- c(3,1,4,2)
x
MergeSort(x, 1, length(x))

