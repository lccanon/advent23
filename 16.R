library(tidyverse)

input <- read_lines("input16") %>%
  str_split("") %>%
  do.call(what = rbind)
input <- cbind(".", rbind(".", input, "."), ".")

compute_energy <- function(input, light) {
  energy <- matrix(FALSE, nrow = nrow(input), ncol = ncol(input))
  energy[light[1,1],light[1,2]] <- TRUE
  light.arch <- array(FALSE, dim = c(nrow(input), ncol(input), 3, 3))
  while (!is.null(light)) {
    new  <- NULL
    for (i in 1:nrow(light)) {
      l <- light[i,]
      if (input[l[1],l[2]] == ".")
        new <- rbind(new, c(l[1] + l[3], l[2] + l[4], l[3:4]))
      else if (input[l[1],l[2]] == "-")
        new <- rbind(new, c(l[1], l[2] - 1, 0, -1),
                           c(l[1], l[2] + 1, 0, 1))
      else if (input[l[1],l[2]] == "|")
        new <- rbind(new, c(l[1] - 1, l[2], -1, 0),
                           c(l[1] + 1, l[2], 1, 0))
      else if (input[l[1],l[2]] == "/")
        new <- rbind(new, c(l[1] - l[4], l[2] - l[3], -l[4:3]))
      else if (input[l[1],l[2]] == "\\")
        new <- rbind(new, c(l[1] + l[4], l[2] + l[3], l[4:3]))
      else
        print("Problem")
    }
    light <- NULL
    for (i in 1:nrow(new)) {
      l <- new[i,]
      if (!light.arch[l[1],l[2],l[3] + 2,l[4] + 2] &&
          !l[1] %in% c(1, nrow(input)) && !l[2] %in% c(1, ncol(input))) {
        light.arch[l[1],l[2],l[3] + 2,l[4] + 2] <- TRUE
        light <- rbind(light, l)
        energy[l[1],l[2]] <- TRUE
      }
    }
  }
  sum(energy)
}

print(compute_energy(input, matrix(c(2, 1, 0, 1), nrow = 1)))

max_energy <- 0
for (ext in c(1, ncol(input))) {
  for (k in 2:(nrow(input) - 1)) {
    print(c(ext, k, max_energy))
    dir <- (ext == 1) - (ext == ncol(input))
    light <- matrix(c(k, ext + dir, 0, dir), nrow = 1)
    max_energy <- max(compute_energy(input, light), max_energy)
    light <- matrix(c(ext + dir, k, dir, 0), nrow = 1)
    max_energy <- max(compute_energy(input, light), max_energy)
  }
}
print(max_energy)
