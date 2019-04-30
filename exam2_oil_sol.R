oil <- readr::read_csv("oil.csv")

## Part 1 ----
(observed_cor <- cor(oil$crude_oil_consumption, oil$temp_change))

## Part 2 ----
n_perms <- 10000
perm_results <- replicate(n_perms, {
  new_oil <- data.frame(
    crude_oil_consumption = oil$crude_oil_consumption, 
    temp_change = sample(oil$temp_change)
  )
  cor(new_oil$crude_oil_consumption, new_oil$temp_change)
})

## Part 3 ----
### Solution 1
hist(perm_results)
abline(v = observed_cor, col = "red")

### Solution 2
library(ggplot2)
ggplot(data.frame(results = perm_results), aes(x = results)) +
  geom_histogram() +
  geom_vline(xintercept = observed_cor, col = "red") +
  theme_bw()

## Part 4 ----
(p_val <- mean(abs(observed_cor) <= abs(perm_results)))
ci <- qnorm(0.975) * sqrt(p_val * (1 - p_val) / length(perm_results))
c(
  lower = p_val - ci,
  p_value = p_val,
  upper = p_val + ci
)

## Part 5 ----
# There is significant positive correlation between crude oil imports and
# average temperature change
