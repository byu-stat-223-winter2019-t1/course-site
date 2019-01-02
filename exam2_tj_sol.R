# Setup ----
tommy_john <- read.csv("tj.csv", header = TRUE, stringsAsFactors = FALSE)
tommy_john <- readr::read_csv("tj.csv")

# Part 1 ----
test_stat <- mean(tommy_john$prek9 - tommy_john$postk9)

# Part 2 ----
n_reps <- 10000
tj_results <- replicate(n_reps, {
  tj_new <- apply(tommy_john[,2:3], 1, sample)
  mean(tj_new[1,] - tj_new[2,])
})

# Part 3 ----
hist(tj_results)
abline(v = test_stat, col = "red")

# Part 4 ----
p_value <- mean(tj_results >= test_stat)
ci <- p_value + c(-1, 1) * qnorm(.975) * sqrt(p_value * (1 - p_value) / 10000)
c(
  lower = ci[1],
  p_value = p_value,
  upper = ci[2]
)

# Part 5 ----
# Since the p-value is insignificant, it seems that signing a pitcher coming off
# of Tommy John surgery is worth it.
