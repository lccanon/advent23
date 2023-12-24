library(tidyverse)

input <- read_lines("input23") %>%
  str_split("") %>%
  do.call(what = rbind)
input <- cbind("#", rbind("#", input, "#"), "#")

DIR <- rbind(c(x = 0, y = 1), c(1, 0), c(0, -1), c(-1, 0))
slopes <- c(">", "v", "<", "^")

max_dist <- (input == "/") * 0
path <- rbind(c(x = 2, y = 3, dir = 2, cost = 0))
while (!is.null(path)) {
  new <- NULL
  for (j in 1:nrow(path)) {
    pp <- path[j,]
    dirs <- NULL
    for (i in 1:nrow(DIR)) {
      if ((i + 1) %% 4 + 1 == pp["dir"])
        next
      if (input[pp["x"] + DIR[i,"x"],pp["y"] + DIR[i,"y"]] %in% c(".", slopes[i]))
        dirs <- c(dirs, i)
    }
    for (k in dirs) {
      x <- pp["x"] + DIR[k,1]
      y <- pp["y"] + DIR[k,2]
      cost <- pp["cost"] + 1
      if (cost > max_dist[x,y]) {
        max_dist[x,y] <- cost
        new <- rbind(new, c(x, y, dir = k, cost))
      }
    }
  }
  path <- new
}
print(max_dist[nrow(max_dist) - 1, ncol(max_dist) - 2])

# locate intersections

inter <- NULL
for (i in 2:(nrow(input) - 1))
  for (j in 2:(ncol(input) - 1))
    if (input[i,j] != "#" && (sum(input[i,j + c(-1, 1)] == "#") + sum(input[i + c(-1, 1),j] == "#")) < 2)
      inter <- rbind(inter, c(x = i, y = j))
inter <- rbind(c(2, 3), inter, c(nrow(input) - 1, ncol(input) - 2))
inter <- cbind(dest = 1:nrow(inter), inter)

# Compute adjacency list

link <- map(1:nrow(inter), as.null)
for (k in 1:nrow(inter)) {
  print(k / nrow(inter))
  path <- rbind(c(inter[k,"x"], inter[k,"y"], dir = 0, cost = 0))
  while (!is.null(path) && nrow(path) > 0) {
    new <- NULL
    for (i in 1:nrow(DIR)) {
      possible <- (i + 1) %% 4 + 1 != path[,"dir"] &
        input[path[,"x"] + DIR[i,1] + (path[,"y"] + DIR[i,2] - 1) * nrow(input)] != "#"
      if (!any(possible))
        next
      sub_path <- rbind(path[possible,])
      sub_path[,"x"] <- sub_path[,"x"] + DIR[i,"x"]
      sub_path[,"y"] <- sub_path[,"y"] + DIR[i,"y"]
      sub_path[,"dir"] <- i
      sub_path[,"cost"] <- sub_path[,"cost"] + 1
      sub_path <- merge(sub_path, inter, all.x = TRUE)
      if (any(!is.na(sub_path[,"dest"])))
        link[[k]] <- rbind(link[[k]], sub_path[!is.na(sub_path[,"dest"]),c("dest", "cost")])
      new <- rbind(new, sub_path[is.na(sub_path[,"dest"]),-ncol(sub_path)])
    }
    path <- unique(new)
  }
}

# library(igraph)
# plot(graph_from_adj_list(map(link, ~ .[,"dest"] %>% unlist)))

# BFS

adj <- map(1:length(link), ~ cbind(src = ., link[[.]])) %>% do.call(what = rbind)
curr <- tibble(pos = 1, step = 0, visited1 = 2 ^ pos, visited2 = 2 ^ 0)
res <- 0
while (nrow(curr) > 0) {
  print(nrow(curr))
  curr <- curr %>%
    inner_join(adj, by = c("pos" = "src"), relationship = "many-to-many") %>%
    filter(!bitwAnd(visited1, 2 ^ ifelse(dest < 30, dest, 0)) |
             !bitwAnd(visited2, 2 ^ ifelse(dest >= 30, dest - 30, 0))) %>%
    mutate(pos = dest) %>%
    mutate(visited1 = bitwOr(visited1, 2 ^ ifelse(pos < 30, pos, 0)),
           visited2 = bitwOr(visited2, 2 ^ ifelse(pos >= 30, pos - 30, 0))) %>%
    mutate(step = step + cost) %>%
    select(-dest, -cost)
  res <- curr %>%
    filter(pos == length(link)) %>%
    summarise(step = max(step)) %>%
    pull %>%
    max(res)
}
print(res)
