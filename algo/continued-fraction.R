# What Continued Fraction in R
# Changes GPL(C) moshahmed@gmail.com

# See http://blogs.sas.com/content/iml/2014/03/14/continued-fraction-expansion-of-pi.html
ContinuedFraction <- function(s){
  n = length(s)
  v = s[n]
  for (i in (n-1):1 ) v = s[i] + 1/v;
  return( v );
}
my.pi    = ContinuedFraction(c(3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1))
my.e     = ContinuedFraction( c(2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10) )
my.sqrt2 = ContinuedFraction( c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) )
my.phi   = ContinuedFraction( c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) )

# From https://stat.ethz.ch/R-manual/R-devel/library/base/html/Constants.html
pi  # built in

## John Machin (ca 1706) computed pi to over 100 decimal places
## using the Taylor series expansion of the second term of
pi - 4*(4*atan(1/5) - atan(1/239))

# moshtag=bignum,gmp,pi
# library("gmp")
# pow.bigz(2,1000)
