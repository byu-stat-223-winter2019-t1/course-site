# Setup ----
assault <- USArrests$Assault

# Find the observed mean of the data set
observed <- mean(assault)

# Part 1 ----
bs_ci <- function(p) {
  # Take a random sample of 10 from p
  x <- sample(p, 10)
  
  # Perform bootstrap sampling
  bs <- replicate(1000, mean(sample(x, replace = TRUE)))
  
  # Return 90% confidence interval
  quantile(bs, c(.05, .95))
}

# Part 2 ----
n_reps <- 5000

# Solution 1
bs_cis <- replicate(n_reps, bs_ci(assault))

# Solution 2
library(future.apply)
plan("multiprocess")
bs_cis <- future_replicate(n_reps, bs_ci(assault))

# Solution 3
library(furrr)
plan("multiprocess")
bs_cis <- future_map(1:n_reps, ~bs_ci(assault))

# Part 3 ----
# Matrix solution (Solutions 1 & 2 from Part 2)
coverage <- mean(apply(bs_cis - observed, 2, prod) < 0)

# List solution (Solution 3 from Part 2)
library(tidyverse)
coverage <- map_lgl(bs_cis, ~.[1] <= observed & observed <= .[2]) %>% 
  mean()

# Part 4 ----
ci <- coverage + c(-1, 1) * qnorm(.975) * sqrt(coverage * (1 - coverage) / n_reps)
c(
  lower = ci[1],
  coverage = coverage,
  upper = ci[2]
)

# Part 5 ----
# .9 is included in the confidence interval for coverage, so it seems that
# coverage is at an appropriate level here.
