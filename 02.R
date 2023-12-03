library(tidyverse)

red <- read_lines("input02") %>%
  str_extract_all("\\d+ red") %>%
  map_int(~ str_extract_all(., "\\d+") %>% as.numeric %>% max)
green <- read_lines("input02") %>%
  str_extract_all("\\d+ green") %>%
  map_int(~ str_extract_all(., "\\d+") %>% as.numeric %>% max)
blue <- read_lines("input02") %>%
  str_extract_all("\\d+ blue") %>%
  map_int(~ str_extract_all(., "\\d+") %>% as.numeric %>% max)
print(sum(which(red > 12 | green > 13 | blue > 14)))
print(sum(red * green * blue))
