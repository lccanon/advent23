library(tidyverse)

input <- read_lines("input08")

ints <- input[[1]] %>%
  str_split_1("") %>%
  match(c("L", "R"))

moves <- tail(input, -2) %>%
  str_extract_all("[0-9A-Z]{3}") %>%
  do.call(what = rbind)
rownames(moves) <- moves[,1]

cycle <- function(ints, moves, input, exits) {
  j <- 0
  while (TRUE) {
    for (i in 1:length(ints)) {
      j <- j + 1
      input <- moves[input,ints[i] + 1]
      if (input %in% exits)
        return(j)
    }
  }
}

print(cycle(ints, moves, "AAA", "ZZZ"))

lcm_cycle <- function(ints, moves, inputs, exits) {
  library(numbers)
  res <- 1
  for (input in inputs)
    res <- LCM(res, cycle(ints, moves, input, exits) / length(ints))
  library(gmp)
  as.bigq(res) * length(ints)
}

inputs <- moves[str_sub_all(rownames(moves), start = 3) %>% unlist == "A",1]
exits <- moves[str_sub_all(rownames(moves), start = 3) %>% unlist == "Z",1]
print(lcm_cycle(ints, moves, inputs, exits))
