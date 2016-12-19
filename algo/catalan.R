# What: Computing catalan numbers in R.
# Changes GPL(C) moshahmed@gmail.com
# 2016-10-14

catalan <- function(n) choose(2*n, n)/(n + 1)
catalan(4)
