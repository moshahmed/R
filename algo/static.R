# What: Static variable counter captured in closure in R
# Changes GPL(C) moshahmed@gmail.com

stato <- function(name=name) {
  count <- 0  # static in closure.
  f <- function(x) {
    count <<- count + 1 # read/write outer count
    # return named list
    return(list(mean=mean(x), count=count, name=name) )
  }
  # return inner function with new copy of count.
  return( f )
}

lee <- stato("lee")
moo <- stato("moo")

l1 <- lee(1:3); l2 <- lee(1:4)
r1 <- moo(1:5); r2 <- moo(1:6)

str(l1); str(l2); str(r1); str(r2)

