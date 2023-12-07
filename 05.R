library(tidyverse)

input <- read_file("input05") %>%
  str_split_1(":\\n?") %>%
  str_split("\\n") %>%
  map(~ str_extract_all(., "\\d+") %>%
        keep(~ length(.) != 0) %>%
        map(as.numeric) %>%
        do.call(what = rbind))

seeds <- input[[2]][1,]
for (i in 3:length(input)) {
  new_seeds <- NULL
  for (s in seeds) {
    new_seed <- NULL
    for (j in seq_len(nrow(input[[i]]))) {
      dest <- input[[i]][j,1]
      src <- input[[i]][j,2]
      range <- input[[i]][j,3]
      if (s >= src && s < src + range)
        new_seed <- s - src + dest
    }
    if (is.null(new_seed))
      new_seeds <- c(new_seeds, s)
    else
      new_seeds <- c(new_seeds, new_seed)
  }
  seeds <- new_seeds
}
print(min(seeds))

seeds <- input[[2]] %>%
  matrix(ncol = 2, byrow = TRUE)
for (i in 3:length(input)) {
  new_ranges <- NULL
  for (k in 1:nrow(seeds)) {
    seeds_start <- seeds[k,1]
    seeds_range <- seeds[k,2]
    seeds_end <- sum(seeds_start + seeds_range - 1)
    for (j in seq_len(nrow(input[[i]]))) {
      dest <- input[[i]][j,1]
      start <- input[[i]][j,2]
      range <- input[[i]][j,3]
      end <- start + range - 1
      if (start <= seeds_start && seeds_end <= end) {
        new_ranges <- rbind(new_ranges, c(seeds_start - start + dest, seeds_range))
        seeds_start <- NULL
        break
      }
      if (start <= seeds_start && seeds_start <= end) {
        new_ranges <- rbind(new_ranges, c(seeds_start - start + dest, end - seeds_start + 1))
        seeds_range <- seeds_range - (end - seeds_start + 1)
        seeds_start <- start + range
      } else if (start <= seeds_end && seeds_end <= end) {
        new_ranges <- rbind(new_ranges, c(dest, seeds_end - start + 1))
        seeds_range <- seeds_range - (seeds_end - start + 1)
      }
    }
    if (!is.null(seeds_start))
      new_ranges <- rbind(new_ranges, c(seeds_start, seeds_range))
  }
  seeds <- new_ranges
}
print(min(seeds[,1]))
