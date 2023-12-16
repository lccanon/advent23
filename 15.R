library(tidyverse)

input <- read_lines("input15") %>%
  str_split_1(",")

hash <- function(val) {
  sum(utf8ToInt(val) * 17 ^ (str_length(val):1)) %% 256
}

map_int(input, hash) %>%
  sum %>%
  print

input <- input %>%
  str_split("(-|=)")

box <- map(1:256, as.null)
for (lens in input) {
  h <- hash(lens[1]) + 1
  if (str_length(lens[2]) == 0) {
    if (!is.null(box[[h]])) {
      idx <- box[[h]][,1] != lens[1]
      box[[h]] <- box[[h]][idx,]
    }
  } else {
    val <- as.integer(lens[2])
    if (any(box[[h]][,1] == lens[1])) {
      box[[h]][box[[h]][,1] == lens[1],2] <- val
    } else
      box[[h]] <- rbind(box[[h]], data.frame(lab = lens[1], val))
  }
}

res <- 0
for (i in 1:length(box)) {
  b <- box[[i]]
  if (!is.null(b))
    res <- res + i * sum(seq_len(nrow(b)) * b[,2])
}
print(res)
