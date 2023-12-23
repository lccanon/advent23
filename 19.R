library(tidyverse)
options(digits = 20)

input <- read_file("input19") %>%
  str_split_1("\\n\\n") %>%
  map(~ str_split_1(., "\\n"))

# Parse workflows
work <- input[[1]]
W <- list()
for (j in 1:length(work)) {
  rule <- work[j] %>%
    str_split_1("[{,}]")
  R <- list()
  for (i in 2:(length(rule) - 2)) {
    rr <- str_split_1(rule[i], ":")
    R[[length(R) + 1]] <- list(var = str_sub(rr[1], end = 1),
                               op = str_sub(rr[1], start = 2, end = 2),
                               thres = str_sub(rr[1], start = 3) %>% as.integer,
                               dest = rr[2])
  }
  R[[length(R) + 1]] <- rule[length(rule) - 1]
  W[[rule[1]]] <- R
}

# Part 1: test each parts
parts <- input[[2]]
res <- 0
for (j in 1:(length(parts) - 1)) {
  # Parse part
  part <- parts[j] %>%
    str_sub(2, -2) %>%
    str_split_1(",")
  P <- list()
  for (i in 1:length(part)) {
    var <- str_sub(part[i], end = 1)
    val <- str_sub(part[i], start = 3) %>% as.integer
    P[[var]] <- val
  }
  # Traverse workflows
  curr <- "in"
  while (!curr %in% c("A", "R")) {
    new <- NULL
    for (i in 1:(length(W[[curr]]) - 1)) {
      var <- W[[curr]][[i]]$var
      op <- W[[curr]][[i]]$op
      thres <- W[[curr]][[i]]$thres
      val <- P[[var]]
      if (op == "<" && val < thres || op == ">" && val > thres) {
        new <- W[[curr]][[i]]$dest
        break
      }
    }
    if (is.null(new))
      curr <- tail(W[[curr]], 1)[[1]]
    else
      curr <- new
  }
  if (curr == "A")
    res <- res + sum(unlist(P))
}
print(res)

# Part 2: compute all valid intervals
interval <- function(W, curr, partial) {
  if (curr == "R")
    return(NULL)
  if (curr == "A") {
    return(prod(partial %>% map(diff) %>% unlist() + 1))
  }
  res <- NULL
  for (i in 1:(length(W[[curr]]) - 1)) {
    var <- W[[curr]][[i]]$var
    op <- W[[curr]][[i]]$op
    thres <- W[[curr]][[i]]$thres
    part <- partial
    if (op == "<")
      part[[var]][2] <- min(thres - 1, partial[[var]][2])
    else
      part[[var]][1] <- max(thres + 1, partial[[var]][1])
    rr <- interval(W, W[[curr]][[i]]$dest, part)
    res <- c(res, rr)
    if (op == "<")
      partial[[var]][1] <- max(thres, partial[[var]][1])
    else
      partial[[var]][2] <- min(thres, partial[[var]][2])
  }
  rr <- interval(W, tail(W[[curr]], 1)[[1]], partial)
  c(res, rr)
}

partial <- list()
for (prop in c("x", "m", "a", "s"))
  partial[[prop]] <- c(1, 4000)
interval(W, "in", partial) %>%
  sum %>%
  print
