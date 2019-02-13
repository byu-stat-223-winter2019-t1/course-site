## Part 1 ----
# spinner function
### Solution 1 ----
spinner <- function() {
  # Initial spin
  spin <- sample(10, 1)
  
  # Number of spins
  n_spins <- 0
  
  # Keep spinning as long as criteria is met
  while (spin %in% c(1, 5, 7) & n_spins <= 50) {
    # New spin
    spin <- sample(10, 1)
    n_spins <- n_spins + 1
  }
  
  # Return 0 if I never won, otherwise, retrun winnings
  if (n_spins == 0) 0
  else 5 * (n_spins) + (n_spins - 1)
}

## Part 2 ----
# Simulation
n_reps <- 10000
results <- replicate(n_reps, spinner())

## Part 3 ----
# Estimation
est <- mean(results >= 20)

## Part 4 ----
# Confidence interval
ci <- est + c(-1, 1) * qnorm(0.975) * sqrt(est * (1 - est) / n_reps)
c(lower = ci[1],
  estimate = est,
  upper = ci[2])


# Coverage
truth <- 0.3 ^ 4
library(future.apply)
plan(multiprocess)
n_reps <- 5000
estimates <- future_replicate(n_reps, {
  mean(replicate(n_reps, spinner()) >= 20)
})

# Confidence intervals
ci <- function(ests, n) {
  lapply(ests, function(est) est + c(-1, 1) * qnorm(0.975) * sqrt(est * (1 - est) / n))
}

cis <- ci(estimates, n_reps)

cov_est <- mean(sapply(cis, function(x) prod(x - truth) < 0))
cov_est + c(-1, 1) * qnorm(0.975) * sqrt(cov_est * (1 - cov_est) / n_reps)
