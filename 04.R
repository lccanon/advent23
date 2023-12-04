library(tidyverse)

input <- read_lines("input04")

input %>%
  str_replace(".*: ", "") %>%
  str_split(" \\| ") %>%
  map(~ str_extract_all(., "\\d+")) -> cards

winning <- list()
numbers <- list()
score <- 0
copy <- rep(1, length(cards))
for (i in 1:length(cards)) {
  winning[[i]] <- as.numeric(cards[[i]][[1]])
  numbers[[i]] <- as.numeric(cards[[i]][[2]])
  gain <- sum(numbers[[i]] %in% winning[[i]])
  if (gain > 0)
    score <- score + 2^(gain - 1)
  copy[i + seq_len(gain)] <- copy[i + seq_len(gain)] + copy[i]
}
print(score)
print(sum(copy))