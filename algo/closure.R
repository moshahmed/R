# What: Closure examples
# Changes GPL(C) moshahmed@gmail.com

# Example 1.
a=1; b=2;
f<-function(a,b) { return( function(x) { a*x + b; }); }
g=f(20,10)
g(2) # 50

# Example 2.
# a,b not captured.
a=10; b=20 # Not used by f.
f<-function(a,b) { function(x) { a*x + b } }
f(7,3)(100) # 703  

# Example 3.
# a,b not captured.
a=10; b=20 # used by f below
f<-function() { function(x) { a*x + b } }
print(f()(100)) # 1020
a=1; b=2 # used by f below
print(f()(100)) # 102

# Example 4.
# http://adv-r.had.co.nz/Functional-programming.html
power <- function(y) { function(x) { x ^ y } }
square <- power(2)
cube <- power(3)
square(2) # 4
square(4) # 16
cube(2) # 8
cube(4) # 64
as.list(environment(square)) # $y 2

# Example 5.
# http://adv-r.had.co.nz/Functional-programming.html
# parent env k is preserved across func calls,
# moshtag=counter,closure
ctr <- function() { k <- 0; function() { k <<- k + 1 ; k; } }
c1 <- ctr()
c2 <- ctr()
c1() # 1
c1() # 2
c2() # 1
c2() # 2

