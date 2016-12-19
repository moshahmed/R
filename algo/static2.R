# What: Static variable in R.
# Changes GPL(C) moshahmed@gmail.com
# http://stackoverflow.com/questions/1088639/static-variables-in-r

f <- function(x) {
    y <- attr(f, "sum")
    if (is.null(y)) {
        y <- 0
    }
    y <- x + y
    attr(f, "sum") <<- y
    return(y)
}
