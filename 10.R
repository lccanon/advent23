library(tidyverse)

input <- read_lines("input10") %>%
  str_split("") %>%
  do.call(what = rbind)

init_pos <- which(input == "S")
input[init_pos] <- "L"

mat <- matrix(-1, nrow = nrow(input), ncol = ncol(input))
mat[init_pos] <- 0

i <- (init_pos - 1) %% nrow(input) + 1 - 1
j <- (init_pos - 1) %/% nrow(input) + 1
while (mat[i,j] == -1) {
  mat[i,j] <- max(mat) + 1
  if (input[i,j] == "|" && mat[i + 1,j] != -1 ||
      input[i,j] == "J" && mat[i,j - 1] != -1 ||
      input[i,j] == "L" && mat[i,j + 1] != -1) {
    # UP
    i <- i - 1
  } else if (input[i,j] == "|" && mat[i - 1,j] != -1 ||
             input[i,j] == "7" && mat[i,j - 1] != -1 ||
             input[i,j] == "F" && mat[i,j + 1] != -1) {
    i <- i + 1
  } else if (input[i,j] == "-" && mat[i,j - 1] != -1 ||
            input[i,j] == "F" && mat[i + 1,j] != -1 ||
            input[i,j] == "L" && mat[i - 1,j] != -1) {
    # RIGHT
    j <- j + 1
  } else if (input[i,j] == "-" && mat[i,j + 1] != -1 ||
             input[i,j] == "7" && mat[i + 1,j] != -1 ||
             input[i,j] == "J" && mat[i - 1,j] != -1) {
    j <- j - 1
  } else
    print("Problem")
}
print((max(mat) + 1) / 2)

input <- ifelse(mat == -1, ".", input)
inside <- 0
for (i in 1:nrow(input)) {
  row <- paste(input[i,], collapse = "") %>%
    str_replace_all("-*", "") %>%
    str_replace_all("F7", "") %>%
    str_replace_all("LJ", "") %>%
    str_replace_all("FJ", "|") %>%
    str_replace_all("L7", "|") %>%
    str_split_1("")
  di <- diff(which(row == "|"))
  if (length(di) > 0)
    inside <- inside + sum(di[seq(1, length(di), 2)] - 1)
}
print(inside)
