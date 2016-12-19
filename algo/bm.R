# What: microbenchmark sorting example
# Changes GPL(C) moshahmed@gmail.com
library(microbenchmark)
library("ggplot2")

x <- runif(100)
tm <- microbenchmark( sqrt(x), x ^ 0.5)
autoplot(tm)

v <- runif(1e4)
tm <- microbenchmark( shell = sort(v, method="shell"), quick = sort(v, method="quick"))
autoplot(tm)

v <- 1:1e5
tm <- microbenchmark( shell = sort(v, method="shell"), quick = sort(v, method="quick"))
autoplot(tm) 
