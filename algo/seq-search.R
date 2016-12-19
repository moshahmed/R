# What: sequential search in R
# Changes GPL(C) moshahmed@gmail.com
# https://visualstudiomagazine.com/articles/2016/09/01/r-language-searching-and-sorting.aspx

seq_search = function(v, t, eps=1.e-5) {
  n <- length(v)
  for (i in 1:n) {
    if (abs(v[i] - t) <= eps) {
      return(i)
    }
  }
  return(0)
}

v <- c(4.5, 0.5, 3.5, 1.5, 5.5, 6.5, 2.5)
seq_search(v, .5)
# 2
# cat("v: \n")
# print(v)

# target <- 0.5
# cat("\ntarget = ", target, "\n")
# idx <- seq_search(v, target, 1.0e-5)
# cat("Using seq_search(), idx = ", idx, "\n\n")

