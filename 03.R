library(tidyverse)

input <- read_lines("input03")
input %>%
  str_split("") %>%
  do.call(what = rbind) %>%
  cbind(".") %>%
  rbind(".") -> mat
input %>%
  str_locate_all("\\d+") -> locs
parts <- 0
for (i in 1:length(locs)) {
  for (j in seq_len(nrow(locs[[i]]))) {
    start <- locs[[i]][j,"start"]
    end <- locs[[i]][j,"end"]
    if (any(str_detect(mat[(i - 1):(i + 1),(start - 1):(end + 1)], "[^.0-9]"))) {
      parts <- parts + as.numeric(str_sub(input[[i]], start, end))
    }
  }
}
print(parts)

parts.loc <- input %>%
  str_locate_all("\\*") %>%
  map2(1:length(input), ~ cbind(row = .y, col = .x[,1])) %>%
  keep(~ ncol(.) == 2) %>%
  do.call(what = rbind) %>%
  as_tibble %>%
  mutate(id = as.character(1:n()))

parts.int <- list()
for (i in 1:length(locs)) {
  for (j in seq_len(nrow(locs[[i]]))) {
    start <- locs[[i]][j,"start"]
    end <- locs[[i]][j,"end"]
    ids <- parts.loc %>%
      filter(row %in% (i - 1):(i + 1) & col %in% (start - 1):(end + 1)) %>%
      pull(id)
    value <- input[[i]] %>% str_sub(start, end) %>% as.numeric
    for (id in ids)
      parts.int[[id]] <- c(parts.int[[id]], value)
  }
}
parts.int %>%
  keep(~ length(.) == 2) %>%
  map_int(prod) %>%
  sum %>%
  print
