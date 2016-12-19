# What: Example of lexical scope.
# Changes GPL(C) moshahmed@gmail.com

y=10 # in lexical scope of f
f<-function(x) { 
  y*x
}
g<-function(x) { 
  y=20 # y in dynamic scope of f not used.
  f(x) 
}
g(3)
# 3*10, not 2*10
