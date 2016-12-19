# What: Norvig spell solution, compare with 2 lines of spell.R
# From http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
# Changes GPL(C) moshahmed@gmail.com
# 2016-09-24

# bigtxt is "Gutenberg EBook of The Adventures of Sherlock Holmes"
bigtxt <-  "http://www.norvig.com/big.txt"
bigtxt <-  "c:/doc3/algo/misc/spell-norvig/big.txt"
bigtxt <-  ".big.txt"

# Read in big.txt, a 6.5 mb collection of different English texts.
raw_text <- paste(readLines(bigtxt), collapse = " ")

# Make the text lowercase and split it up creating a huge vector of word tokens.
split_text <- strsplit(tolower(raw_text), "[^a-z]+")

# Count the number of different type of words.
word_count <- table(split_text)

# Sort the words and create an ordered vector with the most common type of words first.
sorted_words <- names(sort(word_count, decreasing = TRUE))

correct <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, sorted_words)

  # Calculate the minimum edit distance to find a word that exists in big.txt
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 2)

  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])

  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)

  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}

correct('peice') # prints: peace

