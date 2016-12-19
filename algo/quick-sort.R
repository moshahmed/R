# What: Quicksort in R
# From https://www.r-bloggers.com/quicksort-speed-just-in-time-compiling-and-vectorizing/
# Changes GPL(C) moshahmed@gmail.com

quick_sort <- function(A) {
  if (length(A)<2) return(A)
  pivot <- A[sample(length(A),1)]
  c( quick_sort(A[A<pivot]),
     A[A == pivot],
     quick_sort(A[A>pivot]))
}

N <- 1e3 # 1e6 => 10 seconds
A <- rnorm(N)
cat(sprintf("time to quicksort %d numbers\n",N))
print(system.time(quick_sort(A)))

