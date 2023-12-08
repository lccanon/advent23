library(tidyverse)

input <- read_delim("input07", " ", col_names = c("hand", "bid"))

cards.occ <- str_extract_all(input$hand, ".") %>%
  map(~ table(.) %>% sort(decreasing = TRUE) %>% as.integer)
cards <- input %>%
  mutate(nb_card1 = cards.occ %>% map_int(~ head(., 1))) %>%
  mutate(nb_card2 = cards.occ %>% map_int(~ ifelse(length(.) == 1, 0, .[2])))
cards.rank <- str_extract_all(input$hand, ".") %>%
  map(~ match(., rev(c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")))) %>%
  do.call(what = rbind)
cards <- cbind(cards, cards.rank)
cards %>%
  arrange(nb_card1, nb_card2, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(rank = 1:n()) %>%
  mutate(score = bid * rank) %>%
  summarise(bid = sum(score)) %>%
  pull(bid) %>%
  print

cards.occ <- str_extract_all(input$hand, "[^J]") %>%
  map(~ table(.) %>% sort(decreasing = TRUE) %>% as.integer)
cards <- input %>%
  mutate(nb_J = str_count(hand, "J")) %>%
  mutate(nb_card1 = cards.occ %>% map_int(~ ifelse(length(.) == 0, 0, .[1]))) %>%
  mutate(nb_card2 = cards.occ %>% map_int(~ ifelse(length(.) <= 1, 0, .[2])))
cards.rank <- str_extract_all(input$hand, ".") %>%
  map(~ match(., rev(c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")))) %>%
  do.call(what = rbind)
cards <- cbind(cards, cards.rank)
cards %>%
  arrange(nb_card1 + nb_J, ifelse(nb_card2 == 2, 2, 0), `1`, `2`, `3`, `4`, `5`) %>%
  mutate(rank = 1:n()) %>%
  mutate(score = bid * rank) %>%
  summarise(bid = sum(score)) %>%
  pull(bid) %>%
  print
