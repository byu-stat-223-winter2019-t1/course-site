## Part 1 ----
# Create orchard function
orchard <- function(strategy = "highest") {
  # Check strategy
  if (!strategy %in% c("random", "highest")) stop('Strategy must be either "random" or "highest"',
                                                  call. = FALSE)
  # Initialize variables
  fruits <- c(4, 4, 4, 4)
  raven <- 0
  
  # Play the game until it ends
  while (sum(fruits) > 0 & raven < 6) {
    roll <- sample(1:6, 1)
    # 6 is the raven
    if (roll == 6) {
      raven <- raven + 1
      # 5 is wild
    } else if (roll == 5) {
      if (strategy == "highest") {
        fruits[which.max(fruits)] <- fruits[which.max(fruits)] - 1
      } else {
        remaining_fruits <- fruits > 0
        random_fruit <- sample(1:sum(remaining_fruits), 1)
        fruits[remaining_fruits][random_fruit] <- fruits[remaining_fruits][random_fruit] - 1
      }
    } else {
      if (fruits[roll] > 0) fruits[roll] <- fruits[roll] - 1
    }
  }
  
  # Return TRUE if all fruits were collected (sum is 0)
  sum(fruits) == 0
}

## Part 2 ----
# Simulate each strategy 10000 times
### Solution 1
n_reps <- 10000
highest_results <- replicate(n_reps, orchard())
random_results <- replicate(n_reps, orchard("random"))

### Solution 2
n_reps <- 10000
results <- lapply(c("highest", "random"), function(strategy) replicate(n_reps, orchard(strategy)))
names(results) <- c("highest", "random")

## Part 3 ----
# Estimate liklihood of winning
### Solution 1
highest_est <- mean(highest_results)
random_est <- mean(random_results)

### Solution 2
est <- sapply(results, mean)

## Part 4 ----
# Build confidence intervals
### Solution 1
ci <- function(x, alpha = 0.05) {
  p_hat <- mean(x)
  n <- length(x)
  
  ci <- p_hat + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(p_hat * (1 - p_hat) / n)
  
  c(lower = ci[1],
    est = p_hat,
    upper = ci[2])
}

ci(highest_results)
ci(random_results)

### Solution 2
lapply(results, ci)

## Part 5 ----
# Conclusions
# Based on these results, the "highest" strategy is superior since it has a 
# significantly higher probability of winning.