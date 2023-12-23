library(tidyverse)
options(digits = 20)

input <- read_lines("input20") %>%
  str_replace("([%&]?)([a-z]+) -> ", "\\1,\\2,") %>%
  str_split(", ?")

mod <- map_chr(input, ~ .[2])
M <- list()
for (i in mod) {
  M[[i]] <- list()
  M[[i]]$src <- list()
}

for (i in 1:length(input)) {
  conf <- input[[i]]
  type <- conf[1]
  name <- conf[2]
  dest <- tail(conf, -2)
  M[[i]]$type <- type
  M[[i]]$dest <- dest
  for (j in dest)
    M[[j]]$src[[name]] <- "low"
  if (type == "%")
    M[[i]]$state <- FALSE
}

cycle <- NULL
high <- 0
low <- 0
for (i_ in 1:1000000) {
  pulse <- rbind(c("broadcaster", "low"))
  while (!is.null(pulse)) {
    if (i_ <= 1000) {
      high <- high + sum(pulse[,2] == "high")
      low <- low + sum(pulse[,2] == "low")
    }
    new <- NULL
    for (i in 1:nrow(pulse)) {
      name <- pulse[i,1]
      level <- pulse[i,2]
      if (!name %in% mod)
        next
      if (name == "broadcaster") {
        for (d in M[[name]]$dest)
          new <- rbind(new, c(d, level))
      } else if (M[[name]]$type == "%") {
        if (level == "high")
          next
        state <- M[[name]]$state
        for (d in M[[name]]$dest) {
          new_pulse <- ifelse(state, "low", "high")
          new <- rbind(new, c(d, new_pulse))
          M[[d]]$src[[name]] <- new_pulse
        }
        M[[name]]$state <- !state
      } else if (M[[name]]$type == "&") {
        if ("rx" %in% M[[name]]$dest) {
          src <- M[[name]]$src
          if (any(src == "high")) {
            if (is.null(cycle))
              cycle <- (src == "high") * i_
            else if (cycle[src == "high"] == 0)
              cycle[src == "high"] <- i_
          }
        }
        new_pulse <- ifelse(all(M[[name]]$src == "high"), "low", "high")
        for (d in M[[name]]$dest) {
          new <- rbind(new, c(d, new_pulse))
          M[[d]]$src[[name]] <- new_pulse
        }
      }
    }
    pulse <- new
  }
  if (!is.null(cycle) && all(cycle != 0))
    break
}
print(low * high)
library(numbers)
reduce(cycle, LCM)
