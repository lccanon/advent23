library(tidyverse)

input <- read_file("input06") %>%
  str_extract_all("\\d+", simplify = TRUE) %>%
  as.numeric

best <- 1
for (i in 1:(length(input) / 2)) {
  time <- input[i]
  distance <- input[i + length(input) / 2]
  best <- best * sum(0:time * (time - 0:time) > distance)
}
print(best)

# Part 2 with double precision

input <- read_file("input06") %>%
  str_split_1("\\n") %>%
  head(-1) %>%
  str_replace_all("[^0-9]", "") %>%
  as.numeric

time <- input[1]
distance <- input[2]
print(sum(0:time * (time - 0:time) > distance))

# Part 2 with better precision

library(gmp)
library(Rmpfr)

delta <- time ^ 2 - 4 * distance
delta <- delta %>%
  mpfr(log2(delta) + 10) %>%
  sqrt %>%
  as.integer
X <- (time + c(-1, 1) * delta) / 2
print(diff(X) + 1)
