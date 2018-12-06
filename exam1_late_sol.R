# Part 1 ----
on_time <- function() {
  # What maximum time do I have until I'm late
  max_time <- 30
  
  # What time do I leave the apartment
  leave <- runif(1, min = 0, max = 5)
  
  # How long does it take me to walk to the bus station
  walk <- 5
  
  # When does the bus leave the station
  bus_departure <- rnorm(1, 9, 2) + .5
  
  # If I missed the bus, I will not be on time
  if ((walk + leave) > bus_departure) return(FALSE)
  
  # How long does it take the bus to get to campus
  time_to_campus <- 10 + 2 * rbinom(1, 4, .3)
    
  walk_to_class <- 5
  
  # Am I on time?
  (bus_departure + time_to_campus + walk_to_class) <= max_time
}

# Part 2 ----
n_reps <- 10000
results <- replicate(n_reps, on_time())

# Part 3 ----
est <- mean(results)
est

# Part 4 ----
ci <- est + c(-1, 1) * qnorm(0.975) * sqrt(est * (1 - est) / n_reps)
c(
  lower = ci[1],
  estimate = est,
  upper = ci[2]
)

# Part 5 ----
# Since the confidence interval does not contain .7, I need to wake up earlier.