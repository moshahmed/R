# What: R Quiz
# Changes GPL(C) moshahmed@gmail.com

# Quiz Q. What is the output of this R code?
x <- 2
(function(x) { # local x=5
  x <- x+10 # <- local is modified
  cat('a.',x,'\n') # local x=15
  (function(x) { x <<- x+100 })(x+1) # (input=15+1)+100 ->> x 116
  cat('b.',x,'\n') # 116
  x <<- 1000       # parent x=1000 modified
  cat('c.',x,'\n')  # still 116
})(x=x+3) # input x=2+3=5
cat('d.',x,'\n')  # global 1000
# Ans 15, 116, 116, 1000
