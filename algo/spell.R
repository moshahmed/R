# What: Spell checker in R
# From http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
# 2016-09-24

big.txt <- '.big.txt' # was "http://www.norvig.com/big.txt"
sorted_words <- names(sort(table(strsplit(tolower(paste(readLines(big.txt), collapse = " ")), "[^a-z]+")), decreasing = TRUE))

correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }

correct("piese")

# The main reason for why the R version is so short is because base R
# includes the adist function. (A one line spell checker in R is indeed
# possible using the aspell function :)
# 
# A second reason for why the R version is so short is that the many
# vectorized functions in R make it possible to do a lot of work in
# one line.
# 
# Indeed, the horrible line creating the sorted_words vector would be a
# perfect target for some magrittr magic.
# 
# The R version does not solve the problem in exactly the same way as
# Norvig’s code. He maintains the count of each word in the NWORDS variable
# in order to be able to extract the most probable matching word. This is
# not necessary in the R code, as we already have a sorted vector we know
# that the first item always will be the most probable.
