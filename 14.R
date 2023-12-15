library(tidyverse)

input <- read_lines("input14") %>%
  str_split("") %>%
  do.call(what = rbind)
input <- cbind("#", rbind("#", input, "#"), "#")

north <- function(input) {
  for (j in 1:ncol(input))
    for (i in 1:nrow(input))
      if (input[i,j] == "#")
        last_obs <- i
      else if (input[i,j] == "O") {
        input[i,j] <- "."
        input[last_obs + 1,j] <- "O"
        last_obs <- last_obs + 1
      }
  input
}

rotate_right <- function(mat) {
  t(mat)[,ncol(mat):1]
}

loads <- NULL
mat <- list()
cycles <- 1000000000
for (i in 1:cycles) {
  # All tilts
  for (j_ in 1:4)
    input <- north(input) %>% rotate_right
  # Compute load
  load <- sum(apply(input, 1, \(x) sum(x == "O")) * (nrow(input):1 - 1))
  loads <- c(loads, load)
  # Keep track of previous loads
  str <- input %>% unlist %>% paste(collapse = "")
  if (!is.null(mat[[str]])) {
    start <- mat[[str]]
    len <- diff(which(loads == loads[start]))
    print(loads[start + (cycles - start) %% len])
    break
  } else
    mat[[str]] <- i
}
