library(tidyverse)

input <- read_lines("input25") %>%
  str_split(":? ")

nodes <- factor(input %>% unlist %>% unique)

adj <- map(1:length(nodes), as.null)
for (i in 1:length(input)) {
  val <- factor(input[[i]], levels = levels(nodes))
  adj[[val[1]]] <- c(adj[[val[1]]], tail(val, -1))
  for (j in 2:length(val))
    adj[[val[j]]] <- c(adj[[val[j]]], val[1])
}

library(igraph)
gg <- graph_from_adj_list(adj)
# E(gg)$label <- as.character(E(gg))
# E(gg)[c(2726, 1746, 3551)]
# plot(gg)
mc <- graph.mincut(gg, value.only = FALSE)
length(mc$partition1) * length(mc$partition2)
