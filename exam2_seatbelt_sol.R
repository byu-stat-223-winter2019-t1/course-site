seatbelts <- as.data.frame(Seatbelts)

## Part 1 ----
### Solution 1
law <- seatbelts[seatbelts$law == 1,]
no_law <- seatbelts[seatbelts$law == 0,]

law_prop <- sum(law$DriversKilled) / sum(law$drivers)
no_law_prop <- sum(no_law$DriversKilled) / sum(no_law$drivers)

(observed_diff <- no_law_prop - law_prop)

### Solution 2
agg_results <- aggregate(cbind(DriversKilled, drivers) ~ law, sum, data = seatbelts)

agg_results$prop <- agg_results$DriversKilled / agg_results$drivers
(observed_diff <- agg_results$prop[agg_results$law == 0] - agg_results$prop[agg_results$law == 1])

### Solution 3
library(tidyverse)
(observed_diff <- seatbelts %>% 
    group_by(law) %>% 
    summarise(
      total_drivers = sum(drivers),
      total_drivers_killed = sum(DriversKilled),
      p_killed = total_drivers_killed / total_drivers,
    ) %>% 
    select(law, p_killed) %>% 
    spread(key = law, value = p_killed) %>% 
    mutate(diff = `0` - `1`) %>% 
    pull(diff))

## Part 2 ----
### Solution 1
n_samples <- 10000
law <- seatbelts[seatbelts$law == 1,]
no_law <- seatbelts[seatbelts$law == 0,]
bs_results <- replicate(n_samples, {
  law_bs <- sample(nrow(law), replace = TRUE)
  no_law_bs <- sample(nrow(no_law), replace = TRUE)
  
  law_prop_bs <- sum(law[law_bs,]$DriversKilled) / sum(law[law_bs,]$drivers)
  no_law_prop_bs <- sum(no_law[no_law_bs,]$DriversKilled) / sum(no_law[no_law_bs,]$drivers)
  
  no_law_prop_bs - law_prop_bs
})

### Solution 2
bs_results <- replicate(n_samples, {
  seatbelts %>%  
    group_by(law) %>% 
    sample_n(n(), replace = TRUE) %>% 
    summarise(
      total_drivers = sum(drivers),
      total_drivers_killed = sum(DriversKilled),
      p_killed = total_drivers_killed / total_drivers,
    ) %>% 
    select(law, p_killed) %>% 
    spread(key = law, value = p_killed) %>% 
    mutate(diff = `0` - `1`) %>% 
    pull(diff)
})


## Part 3 ----
quantile(bs_results, c(0.025, 0.975))

## Part 4 ----
# The law does not appear to have improved the proportion of injured drivers who were killed