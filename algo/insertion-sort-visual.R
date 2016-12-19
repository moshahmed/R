# What: visualize insertion sort.
# Changes GPL(C) moshahmed@gmail.com
# From https://www.r-bloggers.com/visualizing-sort-algorithms-with-ggplot/
# 2016-09-26 

# install.packages("magrittr")
library("magrittr")
library("ggplot2")
library("dplyr")
library("tidyr")

insertion_sort_steps <- function(x  = sample(1:15)){
  msteps <- matrix(data = x, ncol = length(x))
  for (i in 2:length(x)) {
    j <- i
    while ((j > 1) && (x[j] < x[j - 1])) {
      temp <- x[j]
      x[j] <- x[j - 1]
      x[j - 1] <- temp
      j <- j - 1
      msteps <- rbind(msteps, as.vector(x))
    }
  }
  msteps
}

# Now to test it and see what the function do:

set.seed(12345)
x <- sample(seq(4))
x
## [1] 3 4 2 1
msteps <- insertion_sort_steps(x)

as.data.frame(msteps)

# V1	V2	V3	V4
# 3	4	2	1
# 3	2	4	1
# 2	3	4	1
# 2	3	1	4
# 2	1	3	4
# 1	2	3	4

# Every row is a step in sort the algorithm (a partial sort).
# This matrix is a hard to plot so
# we need a nicer structure. We can transform the matrix to a data_frame
# with the information of every position of every element in each step.

sort_matix_to_df <- function(msteps){
  df <- as.data.frame(msteps, row.names = NULL)
  names(df) <- seq(ncol(msteps))
  df_steps <- df %>%
    tbl_df() %>% 
    mutate(step = seq(nrow(.))) %>% 
    gather(position, element, -step) %>%
    arrange(step)
  df_steps
}

# And we apply this function to the previous steps matrix.

df_steps <- sort_matix_to_df(msteps)

head(df_steps, 10)

# STEP	POSITION	ELEMENT
# 1	1	3
# 1	2	4
# 1	3	2
# 1	4	1
# 2	1	3
# 2	2	2
# 2	3	4
# 2	4	1
# 3	1	2
# 3	2	3

# The next step will be plot the data frame.

plot_sort <- function(df_steps, size = 5, color.low = "#D1F0E1", color.high = "#524BB4"){
  ggplot(df_steps,
         aes(step, position, group = element, color = element, label = element)) +  
    geom_path(size = size, alpha = 1, lineend = "round") +
    scale_colour_gradient(low = color.low, high = color.high) +
    coord_flip() + 
    scale_x_reverse() + 
    theme(legend.position = "none")
}

# Now compare this:

as.data.frame(msteps)

# V1	V2	V3	V4
# 3	4	2	1
# 3	2	4	1
# 2	3	4	1
# 2	3	1	4
# 2	1	3	4
# 1	2	3	4
# With:

plot_sort(df_steps, size = 6) + geom_text(color = "white", size = 4)

# plot of chunk unnamed-chunk-7
# It works, so we can now scroll!

sample(seq(70)) %>% 
  insertion_sort_steps() %>% 
  sort_matix_to_df() %>% 
  plot_sort(size = 2.2)

