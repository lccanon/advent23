library(tidyverse)

input <- read_lines("input04")

input %>%
  str_replace(".*: ", "") %>%
  str_split(" \\| ") %>%
  map(~ str_extract_all(., "\\d+")) -> cards

score <- 0
copy <- rep(1, length(cards))
for (i in 1:length(cards)) {
  gain <- sum(cards[[i]][[1]] %in% cards[[i]][[2]])
  if (gain > 0)
    score <- score + 2^(gain - 1)
  copy[i + seq_len(gain)] <- copy[i + seq_len(gain)] + copy[i]
}
print(score)
print(sum(copy))
