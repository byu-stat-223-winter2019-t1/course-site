# Setup ----
# Actual 2018 data
actual_2018 <- c(
  jan = 200,
  feb = 305,
  mar = 352,
  apr = 327,
  may = 375,
  jun = 407,
  jul = 330,
  aug = 295,
  sep = 275,
  oct = 255,
  nov = 230,
  dec = 450
)

# Part 1 ----
jan_pred <- function(actuals) {
  # Get the index from actuals where the months are jan and dec (ADm-12, ADm-1)
  month_ind <- which(names(actuals) %in% c("jan", "dec"))
  
  # Calculate g (normally distributed with mean 0.25 and sd = 0.15)
  g <- rnorm(1, .25, .15)
  
  # Use FDm formula to calculate forcasted demand
  (1 + (g / 12)) * sum(actuals[month_ind]) / 2
}

jan_pred(actual_2018)

# Part 2 -----
n_reps <- 10000
jan_forecasts <- replicate(n_reps, jan_pred(actual_2018))

# Part 3 ----
est <- mean(jan_forecasts)

# Part 4 -----
ci <- est + c(-1, 1) * qnorm(0.975) * sd(jan_forecasts) / sqrt(length(jan_forecasts))
jan_2019_ci <- c(
  lower = ci[1],
  estimate = est,
  upper = ci[2]
)
jan_2019_ci

# Part 5 ----
# This question was poorly worded. Any of the following solutions is acceptable.
jan_2019_ci / actual_2018["jan"]

(jan_2019_ci - actual_2018["jan"]) / actual_2018["jan"]
