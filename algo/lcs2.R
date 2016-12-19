# What: LCS in R
# Changes GPL(C) moshahmed@gmail.com
# From http://yunus.hacettepe.edu.tr/~iozkan/eco742/lcs/lcss.html
# install.packages("qualV")

library(qualV)
lcs1 <- function(a,b) {
  str2list <- function(str) unlist(strsplit(str,split=""))
  a1 <- str2list("bcabcb")
  b1 <- str2list("abccb")
  return( LCS(a1,b1) )
}
ans <- lcs1("bcabcb", "abccb")

print(ans)
# str(ans)
# ans$LCS
paste(ans$LCS,collapse="") # "abcb"

# LCS in finance from http://yunus.hacettepe.edu.tr/~iozkan/eco742/lcs/lcss.html
# see http://artax.karlin.mff.cuni.cz/r-help/library/qualV/html/LCS.html

# A constructed example
# x <- seq(0, 2 * pi, 0.1)  # time
# y <- 5 + sin(x)           # a process
# o <- y + rnorm(x, sd=0.2) # observation with random error
# p <- y + 0.1              # simulation with systematic bias
# plot(x, o); lines(x, p)
#
# install.packages("sos")
