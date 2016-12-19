# Changes GPL(C) moshahmed@gmail.com

bubble_sort = function(v) {
  n <- length(v)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (v[i] > v[j]) { # swap if bigger
        tmp = v[i]; v[i] = v[j]; v[j] = tmp
      }
    }
  }
  return(v)
}

v <- c(4.5, 0.5, 3.5, 1.5, 5.5, 6.5, 2.5)
bubble_sort(v);

# cat("Sorting v using bubble_sort() \n")
# sv <- bubble_sort(v)
# cat("sv: \n")
# print(sv)
