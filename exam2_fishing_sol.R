# Read data
fishing <- readr::read_csv("fishing.csv")

# Part 1 ----
# Solution 1
strawberry <- fishing$fish[fishing$location == "Strawberry Reservoir"]
mirror <- fishing$fish[fishing$location != "Strawberry Reservoir"]
observed <- median(mirror) - median(strawberry)

# Solution 2
library(tidyverse)
observed <- fishing %>% 
  group_by(location) %>% 
  summarise(median = median(fish)) %>% 
  pull(median) %>% 
  diff()

# Part 2 ----
bs_ci <- function(l, f, n_reps = 10000, alpha = 0.05) {
  bs_results <- replicate(n_reps, {
    f(sample(l[[1]], replace = TRUE)) - f(sample(l[[2]], replace = TRUE))
  })
  quantile(bs_results, c(alpha/2, 1 - alpha/2))
}

location_ci <- bs_ci(list(mirror, strawberry), median)

# Part 3 ----
# Based on the fact that 0 is NOT included in the confidence interval, mirror
# lake looks like it's the better fishing location.

# Part 4 ----
me <- fishing$fish[fishing$fisherman == "Me"]
gpa_j <- fishing$fish[fishing$fisherman != "Me"]
fisherman_ci <- bs_ci(list(me, gpa_j), median)

# Since this confidence interval includes 0, it looks like we're both equal
# fisherman (although my grandpa would certainly say he's the better one).