# What: Mergesort in R
# Changes GPL(C) moshahmed@gmail.com
# from http://stackoverflow.com/questions/26080716/merge-sort-in-r

Merge2<-function(a,b) {
  lenab<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(lenab)) {
    if((ai<=length(a) && a[ai]<b[bi])
      || bi>length(b)) {
      lenab[j] <- a[ai]
      ai <- ai+1
    } else {
      lenab[j] <- b[bi]
      bi <- bi+1          
    }
  }
  lenab
}

MergeSort2<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- MergeSort2(A[1:q])
    b <- MergeSort2(A[(q+1):length(A)])
    Merge2(a,b)
  } else {
    A
  }
}

x<-c(18, 16, 8, 7, 6, 3, 11, 9, 15, 1)
x
MergeSort2(x)
# [1]  1  3  6  7  8  9 11 15 16 18

