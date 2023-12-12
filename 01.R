library(tidyverse)

read_lines("input01") %>%
  str_replace("(one|two|three|four|five|six|seven|eight|nine)", "\\1 \\1") %>%
  str_replace("(.*)(one|two|three|four|five|six|seven|eight|nine)", "\\1\\2 ") %>%
  str_replace_all("one ", "1") %>%
  str_replace_all("two ", "2") %>%
  str_replace_all("three ", "3") %>%
  str_replace_all("four ", "4") %>%
  str_replace_all("five ", "5") %>%
  str_replace_all("six ", "6") %>%
  str_replace_all("seven ", "7") %>%
  str_replace_all("eight ", "8") %>%
  str_replace_all("nine ", "9") %>%
  str_extract_all("\\d") %>%
  map_int(~ str_c(head(., 1), tail(., 1)) %>% as.numeric) %>%
  sum
