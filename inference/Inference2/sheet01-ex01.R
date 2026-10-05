# statistical inference 2
# sheet 1 exercise 1.1
# Topics: Lehmer RNG, inverse transform sampling,
#         Kolmogorov-Smirnov test, chi-square goodness-of-fit test


# ---- (a) ----
# Lehmer random number generator
rng <- function(n, m, a, seed) {

  current <- seed
  result <- numeric(n)

  for (i in seq_len(n)) {
    current <- (a * current) %% m
    result[i] <- current
  }

  # scale to obtain values in (0, 1)
  result / m
}


# Generate U_1, ..., U_n
n <- 1000
m <- 2^31 - 1
a <- 7^5
seed <- 42

u <- rng(
  n = n,
  m = m,
  a = a,
  seed = seed
)


# Check the simulated uniform sample visually
hist(
  u,
  probability = TRUE,
  breaks = 25,
  xlab = "u",
  main = "Lehmer RNG (n = 1000)"
)

abline(h = 1, lwd = 2, lty = 2)



# ---- (b) ----
# Target distribution:
# F(x) = 1 - exp(-2x), x >= 0
#
# Inverse CDF:
# F^{-1}(u) = -1/2 * log(1 - u)

x <- -0.5 * log(1 - u)


# Compare simulated sample with the true Exp(2) density
hist(
  x,
  breaks = 30,
  probability = TRUE,
  xlab = "x",
  main = "Simulated sample and true density"
)

curve(
  dexp(x, rate = 2),
  add = TRUE,
  lwd = 2
)



# ---- (c)(i) ----
# Kolmogorov-Smirnov goodness-of-fit test
#
# H0: X_1, ..., X_n come from Exp(2)
# H1: the distribution differs from Exp(2)

ks_result <- ks.test(
  x,
  "pexp",
  rate = 2
)

ks_result


# Decision at alpha = 0.05
alpha <- 0.05

if (ks_result$p.value < alpha) {
  cat("Reject H0 at the 5% level.\n")
} else {
  cat("Do not reject H0 at the 5% level.\n")
}

# For this sample, the p-value is about 0.038.
# Hence, H0 is rejected at the 5% level.
#
# This can still happen even though the sample was constructed to follow
# Exp(2): a test with significance level 5% may reject a true null hypothesis.
# That is a Type I error.



# ---- (c)(ii) ----
# Chi-square goodness-of-fit test with k = 10 equiprobable classes

k <- 10

# Class boundaries q_j = F^{-1}(j / 10), j = 1, ..., 9
cuts <- c(
  0,
  qexp((1:(k - 1)) / k, rate = 2),
  Inf
)

# Observed counts
observed <- hist(
  x,
  breaks = cuts,
  plot = FALSE
)$counts

# Under H0, each class has probability 0.1
expected <- rep(n / k, k)


# Compute chi-square statistic
chi_square <- sum(
  (observed - expected)^2 / expected
)

# No parameters are estimated from the sample
df <- k - 1

p_value <- 1 - pchisq(
  chi_square,
  df = df
)


cat(
  "Chi-square =", round(chi_square, 3),
  "\ndf =", df,
  "\np-value =", round(p_value, 4),
  "\n"
)


# Decision at alpha = 0.05
if (p_value < alpha) {
  cat("Reject H0 at the 5% level.\n")
} else {
  cat("Do not reject H0 at the 5% level.\n")
}

# Here, the chi-square test does not reject H0.
#
# This is not a contradiction to the K-S result:
# the two tests use different test statistics, and the chi-square test
# additionally loses information by grouping observations into classes.