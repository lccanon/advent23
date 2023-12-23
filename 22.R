library(tidyverse)

input <- read_lines("input22") %>%
  str_split("[,~]") %>%
  map(as.numeric) %>%
  do.call(what = rbind)

mat <- array(0, dim = c(10, 10, max(input) + 1))
for (i in 1:nrow(input)) {
  blk <- input[i,] + 1
  mat[blk[1]:blk[4],blk[2]:blk[5],blk[3]:blk[6]] <- i
}

idx <- sapply(1:dim(mat)[3], \(x) unique(unlist(mat[,,x]))) %>% unlist %>% unique
idx <- idx[idx != 0]
fall <- TRUE
while (fall) {
  fall <- FALSE
  for (i in idx) {
    blk <- input[i,] + 1
    z <- which(mat[blk[1],blk[2],] == i)
    while (min(z - 1) > 1 && all(mat[blk[1]:blk[4],blk[2]:blk[5],z - 1] %in% c(0, i))) {
      z <- z - 1
      fall <- TRUE
    }
    mat[blk[1]:blk[4],blk[2]:blk[5],which(mat[blk[1],blk[2],] == i)] <- 0
    mat[blk[1]:blk[4],blk[2]:blk[5],z] <- i
  }
}

res <- 0
for (i in 1:nrow(input)) {
  cannot <- FALSE
  blk <- input[i,] + 1
  z <- which(mat[blk[1],blk[2],] == i)
  above <- mat[blk[1]:blk[4],blk[2]:blk[5],z + 1]
  for (j in above) {
    if (j == 0 || j == i)
      next
    bl <- input[j,] + 1
    z <- which(mat[bl[1],bl[2],] == j)
    below <- mat[bl[1]:bl[4],bl[2]:bl[5],z - 1]
    if (all(below %in% c(0, i, j))) {
      cannot <- TRUE
      break
    }
  }
  if (!cannot)
    res <- res + 1
}
print(res)

# Part 2

count_fall <- function(input, mat, i) {
  mat[mat == i] <- 0
  idx <- sapply(1:dim(mat)[3], \(x) unique(unlist(mat[,,x]))) %>% unlist %>% unique
  idx <- idx[idx != 0]
  res <- NULL
  fall <- TRUE
  while (fall) {
    fall <- FALSE
    for (id in idx) {
      blk <- input[id,] + 1
      z <- which(mat[blk[1],blk[2],] == id)
      while (min(z - 1) > 1 && all(mat[blk[1]:blk[4],blk[2]:blk[5],z - 1] %in% c(0, id))) {
        z <- z - 1
        res <- c(res, id)
        fall <- TRUE
      }
      mat[blk[1]:blk[4],blk[2]:blk[5],which(mat[blk[1],blk[2],] == id)] <- 0
      mat[blk[1]:blk[4],blk[2]:blk[5],z] <- id
    }
  }
  if (length(unique(res)) != critical[[i]]$falling)
    print(c(i, length(unique(res)), critical[[i]]$falling))
  unique(res)
}
res <- 0
for (i in 1:nrow(input)) {
  print(i / nrow(input))
  res <- res + length(count_fall(input, mat, i))
}
print(res)

# Part 2 more efficient alternative O(V + E)

critical <- map(1:nrow(input), ~ list())
for (height in dim(mat)[3]:1) {
  ids <- unique(unlist(mat[,,height]))
  ids <- ids[ids != 0]
  for (i in ids) {
    if (length(critical[[i]]) != 0)
      next
    # Get successors
    blk <- input[i,] + 1
    z <- which(mat[blk[1],blk[2],] == i)
    above <- mat[blk[1]:blk[4],blk[2]:blk[5],z + 1]
    above <- unique(above[!above %in% c(0, i)])
    # Merge critical nodes
    falling <- 1
    crit <- above
    cont <- TRUE
    while (cont) {
      cont <- FALSE
      for (j in crit) {
        bl <- input[j,] + 1
        z <- which(mat[bl[1],bl[2],] == j)
        below <- mat[bl[1]:bl[4],bl[2]:bl[5],z - 1]
        below <- unique(below[!below %in% c(0, j)])
        if (sum(crit == j) == length(below)) {
          crit <- crit[crit != j]
          cc <- critical[[j]]
          falling <- falling + cc$falling
          crit <- c(crit, cc$node)
          cont <- TRUE
        }
      }
    }
    critical[[i]] <- list(node = crit, falling = falling)
  }
}
map_int(critical, ~ .$falling - 1) %>% sum
