# What: Example of local variable not saved.
# Changes GPL(C) moshahmed@gmail.com

j <- function() {
  if (!exists("y")) {
    y <- 1 # always called
  } else {
    y <- y + 1
  }
  print(y) # 1
}
j() == j() # Always true.
# Prints 11 11 TRUE
