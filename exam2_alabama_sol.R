# Read in data
alabama <- readr::read_csv(here::here("exams/exam2/alabama.csv"))

# Calculate the observed test statistic
# Solution 1
ypp <- function(data) {
  (sum(data$yards_against) / sum(data$points_against)) - (sum(data$yards) / sum(data$points))
}

jalen <- alabama[alabama$starter == "Jalen",]
tua <- alabama[alabama$starter == "Tua",]

jalen_stat <- ypp(jalen)
tua_stat <- ypp(tua)
(observed_statistic <- tua_stat - jalen_stat)

# Solution 2
library(tidyverse)
(observed_statistic <- alabama %>% 
    group_by(starter) %>%
    summarise(
      yppd = sum(yards_against) / sum(points_against),
      yppo = sum(yards) / sum(points),
      ypp_diff = yppd - yppo
    ) %>% 
    pull(ypp_diff) %>% 
    diff())


# Use a permutation test to build a distribution of values under the null hypothesis
n_permutations <- 10000

# Solution 1
new_data <- alabama
results <- replicate(n_permutations, {
  new_data$starter <- sample(new_data$starter)
  
  jalen <- new_data[new_data$starter == "Jalen",]
  tua <- new_data[new_data$starter == "Tua",]
  
  jalen_stat <- ypp(jalen)
  tua_stat <- ypp(tua)
  
  tua_stat - jalen_stat
})

# Solution 2
library(tidyverse)
results <- replicate(n_permutations, {
  alabama %>% 
    # Shuffle starters
    mutate(starter = sample(starter)) %>% 
    # Calculate the metric by starter
    group_by(starter) %>%
    summarise(
      yppd = sum(yards_against) / sum(points_against),
      yppo = sum(yards) / sum(points),
      ypp_diff = yppd - yppo
    ) %>% 
    pull(ypp_diff) %>% 
    diff()
})

# Plot a histogram of the resulting distribution along with a vertical line 
# representing the observed test statistic
hist(results)
abline(v = observed_statistic, col = "red")

# Caclculate a p-value using the results of your permutation test and the observed
# test statistic
p_value <- mean(results >= observed_statistic)

# Build a 95% confidence interval around that p-value
ci <- p_value + c(-1, 1) * qnorm(.975) * sqrt(p_value * (1 - p_value) / length(results))
c(lower = ci[1],
  p_value = p_value,
  upper = ci[2])

# Given this test, what do we conclude?
# Given this test, we fail to reject the null hypotesis. It appears Alabama is 
# equally as dominate with Tua or Jalen at the helm (Although I remain partial
# to Tua, I mean, come on, that arm!).