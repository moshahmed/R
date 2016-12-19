# What: Fibonacci with memoization in r
# from http://stackoverflow.com/questions/31683471/fibonacci-memoization-in-r
# Changes GPL(C) moshahmed@gmail.com
# 2016-09-24 

fib <- function(n) {
    if (length(n) > 1) return(sapply(n, fib)) # accept a numeric vector
    if (n == 1 || n==2) return(1)
    return(fib(n-1)+fib(n-2)) # use recursion
}

# Verify the Fibonacci sequence 1 through 10
(actual <- fib(1:10))
(expected <- c(1,1,2,3,5,8,13,21,34,55))
all.equal(actual,expected)

system.time(fib(1:20))
# system.time(fib(1:200)) .. wont finish

# Closed form is very fast.
vast = function(n) round(((5 + sqrt(5)) / 10) * (( 1 + sqrt(5)) / 2) ** (1:n - 1))
vast(10)
# [1]  1  1  2  3  5  8 13 21 34 55

# Very fast with memoization
vast2=function(n) {
  x = numeric(n)
  x[1:2] = c(1,1)
  for(i in 3:n) x[i] = x[i-1] + x[i-2]
  return(x)
}
vast2(10)

# Very fast.
fibonacci <- function(n) {
    x <- c(0,1)
    while (length(x) < n) {
        position <- length(x)
        new <- x[position] + x[position-1]
        x <- c(x,new)
    }
    return(x)
}
fibonacci(20)

