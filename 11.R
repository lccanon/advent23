library(tidyverse)

input <- read_lines("input11") %>%
  str_split("") %>%
  do.call(what = rbind)

expansion <- 1000000
for (i in 1:nrow(input))
  if (all(input[i,] == "."))
    input[i,] <- expansion
for (i in 1:ncol(input))
  if (all(input[,i] == "." | input[,i] == expansion))
    input[,i] <- expansion

stars <- which(input == "#")

res <- 0
for (star1 in stars) {
  x1 <- (star1 - 1) %% nrow(input) + 1
  y1 <- (star1 - 1) %/% nrow(input) + 1
  for (star2 in stars) {
    if (star1 <= star2)
      next
    x2 <- (star2 - 1) %% nrow(input) + 1
    y2 <- (star2 - 1) %/% nrow(input) + 1
    dist <- abs(x1 - x2) + abs(y1 - y2) + (expansion - 1) *
      (sum(input[x1:x2,y1] == expansion) + sum(input[x1, y1:y2] == expansion))
    res <- res + dist
  }
}
print(res)
