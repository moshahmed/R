# What Binary search
# Changes GPL(C) moshahmed@gmail.com

bin_search = function(v, t, eps) {
  lo <- 1; hi <- length(v)
  while (lo <= hi) {
    mid <- as.integer(round((lo + hi) / 2)) # always even!
    if (abs(v[mid] - t) <= eps) {
      return(mid)
    } else if (v[mid] < t) { # C style would be OK
      lo <- mid + 1
    } else {
      hi <- mid - 1
    }
  }
  return(0)
}

worst_sort = function(v) {
  n <- length(v)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (v[i] > v[j]) {
        tmp = v[i]; v[i] = v[j]; v[j] = tmp
      }
    }
  }
  return(v)
}

 
v <- c(4.5, 0.5, 3.5, 1.5, 5.5, 6.5, 2.5)
sv <- worst_sort(v)
# sv : (0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
target <- 2.5
cat("\ntarget = ", target, "\n")
idx <- bin_search(sv, target, 1.0e-5)
cat("Using bin_search(), idx = ", idx, "\n")

# Using bin_search(), idx =  3 
