# What: Memoized recursive fibonacci function
# Changes GPL(C) moshahmed@gmail.com

memo <- c(NA)
f <- function (n){
  if(n < 2) return(1)
  cat("Call f(",n,")\n")
  if(!is.na(memo[n])) return(memo[n])
  s = f(n-1) + f(n-2)
  # memo[n] <<- s
  cat("Call f(",n,") returning",s,"\n")
  return(s)
}
print(f(4))
