library(tidyverse)

input <- read_lines("input12") %>%
  str_split(" ")

res <- 0
for (i in 1:length(input)) {
  row <- input[[i]]
  groups <- str_split_1(row[2], ",") %>%
    as.numeric
  states <- str_split_1(row[1], "")
  states <- rep(c(states, "?"), 5) %>% head(-1)
  groups <- rep(groups, 5)
  # Dynamic programming : given the i first states and j first groups,
  # compute recursively the number of arrangements
  cc <- matrix(0, nrow = length(states), ncol = length(groups))
  for (i in 1:length(states))
    for (j in 1:length(groups)) {
      if (i < sum(groups[1:j]))
        next
      if (i == groups[j]) {
        cc[i,j] <- all(states[1:i] %in% c("?", "#"))
        next
      }
      # Either current state is not used by a group
      if (states[i] %in% c("?", ".") && i > 1)
        cc[i,j] <- cc[i - 1,j]
      # Or it is used (and the previous state must not belong to a group)
      if (all(states[(i - groups[j] + 1):i] %in% c("?", "#")) && states[i - groups[j]] %in% c("?", ".")) {
        if (j == 1)
          cc[i,j] <- cc[i,j] + all(states[1:(i - groups[j])] %in% c("?", "."))
        else {
          if (i != groups[j] + 1)
            cc[i,j] <- cc[i,j] + cc[i - groups[j] - 1,j - 1]
        }
      }
    }
  CC <- cc[length(states),length(groups)]
  res <- res + CC
}

library(gmp)
print(as.bigq(res))
