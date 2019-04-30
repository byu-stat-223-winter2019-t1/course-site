brady <- readr::read_csv("brady.csv")

## Part 1 ----
### Solution 1
ypg <- aggregate(yards ~ game_type, mean, data = brady)
(observed_diff <- ypg$yards[ypg$game_type == "Playoffs"] - ypg$yards[ypg$game_type == "Regular"])

### Solution 2
library(tidyverse)
(observed_diff <- brady %>% 
  group_by(game_type) %>% 
  summarise(avg_ypg = mean(yards)) %>% 
  spread(key = game_type, value = avg_ypg) %>% 
  mutate(diff = Playoffs - Regular) %>% 
  pull(diff))

## Part 2 ----
n_perms <- 10000
### Solution 1
new_brady <- brady
perm_results <- replicate(n_perms, {
  new_brady$game_type <- sample(new_brady$game_type)
  ypg <- aggregate(yards ~ game_type, mean, data = new_brady)
  ypg$yards[ypg$game_type == "Playoffs"] - ypg$yards[ypg$game_type == "Regular"]
})

### Solution 2
perm_results <- replicate(n_perms, {
  brady %>% 
    mutate(game_type = sample(game_type)) %>% 
    group_by(game_type) %>% 
    summarise(avg_ypg = mean(yards)) %>% 
    spread(key = game_type, value = avg_ypg) %>% 
    mutate(diff = Playoffs - Regular) %>% 
    pull(diff)
})

## Part 3 -----
(p_val <- mean(perm_results >= observed_diff))
ci <- qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
c(
  lower = p_val - ci,
  p_value = p_val,
  upper = p_val + ci
)
