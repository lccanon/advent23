library(tidyverse)

input <- read_lines("input17") %>%
  str_split("") %>%
  map(as.numeric) %>%
  do.call(what = rbind)

DIR <- rbind(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))
MIN_LEN <- 4
MAX_LEN <- 10

loss <- array(Inf, dim = c(nrow(input), ncol(input), 4, MAX_LEN))
pos <- rbind(c(1, 2, 1, 1), c(2, 1, 2, 1))
loss[1,2,1,1] <- input[1,2]
loss[2,1,2,1] <- input[2,1]

best <- Inf
while (!is.null(pos)) {
  print(c(nrow(pos), best, max(pos)))
  new <- NULL
  for (i in 1:nrow(pos)) {
    p <- pos[i,]
    lp <- loss[p[1],p[2],p[3],p[4]]
    for (j in 1:nrow(DIR)) {
      samedir <- p[3] == j
      pn <- c(p[1] + DIR[j,1], p[2] + DIR[j,2], j, samedir * p[4] + 1)
      if (pn[1] %in% c(0, nrow(input) + 1) || pn[2] %in% c(0, ncol(input) + 1) ||
          ((p[3] + 2) %% 4 == j %% 4) || (p[4] < MIN_LEN && !samedir) || (p[4] == MAX_LEN && samedir))
        next
      if (lp + input[pn[1],pn[2]] < min(best, loss[pn[1],pn[2],pn[3],pn[4]])) {
        new <- rbind(new, pn)
        loss[pn[1],pn[2],pn[3],pn[4]] <- lp + input[pn[1],pn[2]]
        if (pn[1] == nrow(input) && pn[2] == ncol(input) && pn[4] >= MIN_LEN)
          best <- loss[pn[1],pn[2],pn[3],pn[4]]
      }
    }
  }
  pos <- unique(new)
}
print(best)

# Vectorized alternative (twice faster)

input <- read_lines("input17") %>%
  str_split("") %>%
  map(as.numeric) %>%
  do.call(what = rbind)
colnames(input) <- 1:ncol(input)
input <- input %>%
  as_tibble %>%
  mutate(row = 1:n()) %>%
  pivot_longer(-row) %>%
  rename(col = name) %>%
  mutate_at("col", as.integer)

DIR <- rbind(c(0, 1), c(1, 0), c(0, -1), c(-1, 0)) %>%
  as_tibble %>%
  rename(dx = V1, dy = V2)
MIN_LEN <- 4
MAX_LEN <- 10
pos <- tibble(row = 1:2, col = 2:1, dx_prev = 0:1, dy_prev = 1:0, len = 1) %>%
  inner_join(input, by = c("row", "col")) %>%
  rename(loss = value)
best <- Inf
archive <- pos
while (nrow(pos) > 0) {
  print(c(nrow(pos), best, max(pos$row), nrow(archive)))
  pos <- pos %>%
    expand_grid(DIR) %>%
    filter(dx + dx_prev != 0 | dy + dy_prev != 0) %>%
    filter(len >= MIN_LEN | dx == dx_prev & dy == dy_prev) %>%
    mutate(len = 1 + ifelse(dx == dx_prev & dy == dy_prev, len, 0)) %>%
    mutate(row = row + dx, col = col + dy, dx_prev = dx, dy_prev = dy) %>%
    filter(len <= MAX_LEN) %>%
    inner_join(input, by = c("row", "col")) %>%
    mutate(loss = loss + value) %>%
    left_join(archive, by = c("row", "col", "dx_prev", "dy_prev", "len"),
              relationship = "many-to-many") %>%
    group_by(row, col, dx_prev, dy_prev, len) %>%
    summarise(loss.x = min(loss.x), loss.y = min(loss.y), .groups = "drop") %>%
    filter(is.na(loss.y) | loss.x < loss.y) %>%
    select(-loss.y) %>%
    rename(loss = loss.x)
  archive <- rbind(archive, pos)
  if (nrow(archive) > 1.5e6)
    archive <- archive %>%
      group_by(row, col, dx_prev, dy_prev, len) %>%
      summarise(loss = min(loss), .groups = "drop")
  best <- pos %>%
    filter(row == max(input$row) & col == max(input$col) & len >= MIN_LEN) %>%
    pull(loss) %>%
    min(Inf, best)
  pos <- pos %>%
    filter(loss < best)
}
print(best)
