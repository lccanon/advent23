library(tidyverse)

input <- read_file("input13") %>%
  str_split_1("\\n\\n")

score_col <- function(pat, diff) {
  for (j in 1:(ncol(pat) - 1))
    if (sum(pat[,max(1, 2 * j - ncol(pat) + 1):j] !=
            pat[,min(ncol(pat), 2 * j):(j + 1)]) == diff)
      return(j)
  -1
}

score <- function(pat, diff) {
  max(score_col(pat, diff), 100 * score_col(t(pat), diff))
}

res <- 0
for (i in 1:length(input)) {
  pat <- input[[i]] %>%
    str_split_1("\\n") %>%
    str_split("") %>%
    do.call(what = rbind)
  res <- res + score(pat, 1)
}
print(res)
