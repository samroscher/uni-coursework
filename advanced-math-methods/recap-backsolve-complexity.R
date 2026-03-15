# advanced mathematical methods
# Repetition Sheet WS 24/25 - Exercise 2
# Topic: Computational Complexity - Backward Substitution

# The computational complexity of backward substitution for a linear system
# with n equations is O(n^2). Verify this empirically in R:
# For various values of n, create an upper triangular matrix of ones
# (A <- matrix(1, n, n); A[lower.tri(A)] <- 0) and a random vector b of
# length n. Use microbenchmark::microbenchmark() to measure the runtime of
# backsolve(A, b) and plot the results.
library(ggplot2)

n <- seq(100, 2000, by = 100)

upper_mat <- function(n) {
  A <- matrix(1, n, n)
  A[lower.tri(A)] <- 0
  A
}

set.seed(1)
matrices <- lapply(n, upper_mat)
b <- lapply(n, rnorm)

data <- data.frame(n = numeric(), median_time = numeric())
for (i in seq_along(n)) {
  mb <- microbenchmark::microbenchmark(backsolve(matrices[[i]], b[[i]]))
  data[i, "median_time"] <- median(mb$time)
}
data$n <- n

# plot
ggplot(data, aes(x = n, y = median_time)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Backward Substitution: Runtime vs. n",
    x = "n",
    y = "Median Runtime"
  )

ggplot(data, aes(x = n, y = sqrt(median_time))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Backward Substitution: sqrt(Runtime) vs. n",
    x = "n",
    y = "sqrt(Median Runtime)"
  )

# Results:
# n = 100;  6724.0 ns
# n = 500;  52890.0 ns
# n = 1000; 181015.0 ns
# n = 2000; 693822.5 ns

# Session Info:
# R version: 4.5.2 (2025-10-31)
# Platform: aarch64-apple-darwin20 (Apple Silicon)
# OS: macOS Sequoia 15.6
# microbenchmark version: 1.5.0
# ggplot2 version: 3.5.2
# LAPACK version: 3.12.1
# Hardware: Apple M-series (ARM64)