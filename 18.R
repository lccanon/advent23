library(tidyverse)
library(gmp)

hold <- function(input) {
  x <- 0
  y <- 0
  area <- as.bigq(0)
  for (i in 1:nrow(input)) {
    # RIGHT, DOWN, LEFT, RIGHT
    if (input[i,]$dir == 0)
      y <- y + input[i,]$len
    else if (input[i,]$dir == 1)
      x <- x + input[i,]$len
    else if (input[i,]$dir == 2)
      y <- y - input[i,]$len
    else if (input[i,]$dir == 3)
      x <- x - input[i,]$len
    else
      print("Problem")
    if (input[i,]$dir %% 2 == 0)
      area <- area - x * y
    else
      area <- area + x * y
  }
  area + sum(input$len) / 2 + 1
}

input <- read_lines("input18") %>%
  str_split(" ") %>%
  do.call(what = rbind) %>%
  as_tibble %>%
  rename(dir = V1, len = V2, color = V3) %>%
  mutate_at("len", as.integer) %>%
  mutate(dir = match(dir, c("R", "D", "L", "U")) - 1)
print(hold(input))

input <- read_lines("input18") %>%
  str_replace_all(".*([0-9a-z]{5})([0-3]).*", "0x\\1 \\2") %>%
  str_split(" ") %>%
  do.call(what = rbind) %>%
  as_tibble %>%
  rename(dir = V2, len = V1) %>%
  mutate_at("len", strtoi) %>%
  mutate_at("dir", as.integer)
print(hold(input))

# Vectorized alternative

DIR <- tibble(dir = 0:3, x = c(0, 1, 0, -1), y = c(1, 0, -1, 0))
input %>%
  inner_join(DIR, by = "dir") %>%
  mutate(x = cumsum(x * len), y = cumsum(y * len)) %>%
  mutate(area = x * y * ifelse(dir %% 2 == 0, -1, 1)) %>%
  summarise(area = sum(area) + sum(len) / 2 + 1) %>%
  pull %>%
  as.bigq
