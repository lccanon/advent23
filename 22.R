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

count_fall <- function(input, mat, i = NULL) {
  if (!is.null(i))
    mat[mat == i] <- 0
  idx <- sapply(1:dim(mat)[3], \(x) unique(as.vector(mat[,,x]))) %>% unlist %>% unique
  idx <- idx[idx != 0]
  res <- 0
  fall <- TRUE
  while (fall) {
    fall <- FALSE
    for (id in idx) {
      blk <- input[id,] + 1
      z_init <- which(mat[blk[1],blk[2],] == id)
      z <- z_init
      while (min(z - 1) > 1 && all(mat[blk[1]:blk[4],blk[2]:blk[5],z - 1] %in% c(0, id))) {
        z <- z - 1
        fall <- TRUE
      }
      if (any(z != z_init))
        res <- res + 1
      mat[blk[1]:blk[4],blk[2]:blk[5],z_init] <- 0
      mat[blk[1]:blk[4],blk[2]:blk[5],z] <- id
    }
  }
  list(mat = mat, fall = res)
}

mat <- count_fall(input, mat)$mat
falls <- NULL
for (i in 1:nrow(input)) {
  print(i / nrow(input))
  falls <- c(falls, count_fall(input, mat, i)$fall)
}
print(sum(falls == 0))
print(sum(falls))

# Part 1 more efficient alternative

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

# Part 2 more efficient alternative O(V + E)

critical <- map(1:nrow(input), ~ list())
for (height in (dim(mat)[3] - 1):2) {
  ids <- unique(as.vector(mat[,,height]))
  for (i in ids[ids != 0]) {
    if (length(critical[[i]]) != 0)
      next
    # Get successors
    blk <- input[i,] + 1
    z <- which(mat[blk[1],blk[2],] == i)
    above <- mat[blk[1]:blk[4],blk[2]:blk[5],z + 1]
    above <- unique(above[!above %in% c(0, i)])
    # Merge critical nodes (unmoved parts)
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

# Part 2 alternative with dominator tree from igraph

adj <- list()
for (height in 2:(dim(mat)[3] - 1)) {
  ids <- unique(as.vector(mat[,,height]))
  for (i in ids[ids != 0]) {
    # Get successors
    blk <- input[i,] + 1
    z <- which(mat[blk[1],blk[2],] == i)
    above <- mat[blk[1]:blk[4],blk[2]:blk[5],z + 1]
    above <- unique(above[!above %in% c(0, i)])
    adj[[i]] <- above
  }
}
base <- unique(as.vector(mat[,,2]))
adj[[length(adj) + 1]] <- base[base != 0]

library(igraph)
domtree <- dominator_tree(graph_from_adj_list(adj), length(adj))$dom
idx <- sapply(dim(mat)[3]:1, \(x) unique(as.vector(mat[,,x]))) %>% unlist %>% unique
falling <- rep(1, length(adj))
for (i in idx[idx != 0])
  falling[domtree[i]] <- falling[domtree[i]] + falling[i]
print(sum(falling) - tail(falling, 1) - nrow(input))
