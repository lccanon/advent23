library(tidyverse)

red <- read_lines("input02") %>%
  str_extract_all("\\d+ red") %>%
  map(~ str_extract_all(., "\\d+") %>% as.numeric %>% max) %>%
  unlist
green <- read_lines("input02") %>%
  str_extract_all("\\d+ green") %>%
  map(~ str_extract_all(., "\\d+") %>% as.numeric %>% max) %>%
  unlist
blue <- read_lines("input02") %>%
  str_extract_all("\\d+ blue") %>%
  map(~ str_extract_all(., "\\d+") %>% as.numeric %>% max) %>%
  unlist
print(sum(which(red > 12 | green > 13 | blue > 14)))
print(sum(red * green * blue))
