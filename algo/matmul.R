# What: Standard matrix multiplication algorithm.
# Changes GPL(C) moshahmed@gmail.com
# For matrix primitives see http://www.johnmyleswhite.com/notebook/2009/12/16/quick-review-of-matrix-algebra-in-r/

matmul <- function(A,B) {
    da = dim(A)
    db = dim(B)
    if( da[2] != db[1] ) stop("bad dim")
    d <- matrix(0, nrow = da[1], ncol = db[2])
    for (i in 1:da[1] )
      for (j in 1:db[2]) {
        tot = 0
        for( k in 1:da[2] )
          tot = tot + A[i, k] * B[k, j]
        d[i,j] <- tot
      }
    return(d)
}
A <- matrix(2:10, 3, 4)
B <- matrix(1:10, 4, 3)
C <- matmul( A, B)

