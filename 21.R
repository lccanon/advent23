library(tidyverse)
options(digits = 20)

input <- read_lines("input21") %>%
  str_split("") %>%
  do.call(what = rbind)

propa <- function(input, position, iter = Inf) {
  pos <- input
  pos[position[1], position[2]] <- "O"
  res <- NULL
  repeat {
    res <- c(res, sum(pos == "O"))
    if (length(res) >= iter || length(res) >= 3 && tail(res, 3) %>% head(1) == tail(res, 1))
      return(res)
    pos <- ifelse(input != "#" & (rbind(FALSE, pos[1:(nrow(pos) - 1),] == "O") |
                                    rbind(pos[2:nrow(pos),], FALSE) == "O" |
                                    cbind(FALSE, pos[,1:(ncol(pos) - 1)] == "O") |
                                    cbind(pos[,2:ncol(pos)], FALSE) == "O"), "O", ".")
  }
}

start_x <- (which(input == "S") - 1) %% ncol(input) + 1
start_y <- (which(input == "S") - 1) %/% ncol(input) + 1
print(propa(input, c(start_x, start_y), 64 + 1) %>% tail(1))

# Part 2 with polynomial regression

N <- 26501365
size <- nrow(input)
size.half <- nrow(input) %/% 2

input.empty <- input
input.empty[input == "S"] <- "."
iter <- 4
input.large <- input
input.EMPTY <- input.empty
for (i in seq_len(iter))
  input.large <- rbind(input.empty, input.large, input.empty)
for (i in seq_len(iter))
  input.EMPTY <- rbind(input.empty, input.EMPTY, input.empty)
for (i in seq_len(iter))
  input.large <- cbind(input.EMPTY, input.large, input.EMPTY)

start_x_large <- (which(input.large == "S") - 1) %% ncol(input.large) + 1
start_y_large <- (which(input.large == "S") - 1) %/% ncol(input.large) + 1
prop <- propa(input.large, c(start_x_large, start_y_large), size.half + size * iter + 1)

steps <- prop[1 + size.half + size * 2:iter]
lm(steps ~ poly(iter, 2, raw = TRUE), data = data.frame(iter = 2:iter, steps = steps)) %>%
  predict(newdata = data.frame(iter = N %/% size)) %>%
  round %>%
  print

# Part 2 with manual analysis

PROPA <- list()
PROPA[["center"]] <- propa(input, c(start_x, start_y))
PROPA[["SE"]] <- propa(input, c(nrow(input), ncol(input)), size.half + size)
PROPA[["S"]] <- propa(input, c(nrow(input), start_y), size)
PROPA[["SO"]] <- propa(input, c(nrow(input), 1), size.half + size)
PROPA[["O"]] <- propa(input, c(start_x, 1), size)
PROPA[["NO"]] <- propa(input, c(1, 1), size.half + size)
PROPA[["N"]] <- propa(input, c(1, start_y), size)
PROPA[["NE"]] <- propa(input, c(1, ncol(input)), size.half + size)
PROPA[["E"]] <- propa(input, c(start_x, ncol(input)), size)

comp <- tail(PROPA[["center"]], 2)
iter <- N %/% size
(PROPA[["E"]][size] + iter * PROPA[["SE"]][size.half] + (iter - 1) * PROPA[["SE"]][size.half + size] +
    PROPA[["S"]][size] + iter * PROPA[["SO"]][size.half] + (iter - 1) * PROPA[["SO"]][size.half + size] +
    PROPA[["O"]][size] + iter * PROPA[["NO"]][size.half] + (iter - 1) * PROPA[["NO"]][size.half + size] +
    PROPA[["N"]][size] + iter * PROPA[["NE"]][size.half] + (iter - 1) * PROPA[["NE"]][size.half + size] +
    iter ** 2 * comp[1] + (iter - 1) ** 2 * comp[2]) %>%
  print
