# What: Microbenchmark compare two mergesort in R
# Changes GPL(C) moshahmed@gmail.com

# install.packages("microbenchmark")
library(microbenchmark)

m1<-function() {
    x<-sample(1000, 250);
    mmergesort(x)
}

m2<-function() {
    x<-sample(1000, 250);
    MergeSort(x, 1, length(x))
}

microbenchmark(m1(), m2())

# Unit: milliseconds
#  expr  min   lq mean median   uq  max neval cld
#  m1() 10.2 10.3 10.8   10.5 10.8 15.2   100  a 
#  m2() 12.8 13.0 13.6   13.4 14.5 15.7   100   b

library("ggplot2")
tm <- microbenchmark(m1(), m2())
autoplot(tm)
# *graphic*
# m1 is faster.
