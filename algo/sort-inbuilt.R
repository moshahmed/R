# What: Compare inbuilt sorting methods with system.time in R
# Changes GPL(C) moshahmed@gmail.com
# from https://visualstudiomagazine.com/articles/2016/09/01/r-language-searching-and-sorting.aspx  

# The sort function, like almost all regular R functions, passes
# parameters by value rather than by reference. In other words, a sorted
# copy of the original vector is returned, and the original vector isn't
# changed. If you really want to sort a vector in-place rather than get
# a new vector, you can use the calling pattern:

# cat("Sorting v using built-in sort(array,method)\n")
# method={auto,radix,shell,quick}

v <- c(4, 0, 3, 6, 5, 1, 2)
sort(v, method="shell")
# cat("sv: \n")
# print(sv)

## A larger test
x <- rnorm(1e7)
system.time(x1 <- sort(x, method = "shell"))
system.time(x2 <- sort(x, method = "quick"))
system.time(x3 <- sort(x, method = "radix"))
stopifnot(identical(x1, x2))
stopifnot(identical(x1, x3))

