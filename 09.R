library(tidyverse)

input <- read_lines("input09") %>%
  str_split(" ") %>%
  map(as.numeric) %>%
  do.call(what = rbind)

extrapolate <- function(x) {
  if (all(x == 0))
    return (0)
  tail(x, 1) + extrapolate(diff(x))
}

output <- apply(input, 1, function(x) extrapolate(x) %>% tail(1))
print(sum(output))
output <- apply(input, 1, function(x) rev(x) %>% extrapolate %>% tail(1))
print(sum(output))
