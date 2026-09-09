# statistical inference 1
# sheet 01 exercise 1.3
# Topics: Central Limit Theorem, Binomial as sum of Bernoulli,
#         simulation, histogram vs. normal approximation

# Given X ~ Bin(100, 0.75), we first simulate X directly (a), since X is
# itself already a sum of n = 100 iid Bernoulli(0.75) variables. We then
# write functions plotting r standardised sample means against the
# standard normal, for Bernoulli (b) and Exp(lambda) (c) draws.

set.seed(1)

# ---- (a) ----
n <- 100
p <- 0.75
r <- 1000

binom_var <- rbinom(r, size = n, prob = p)

# CLT: X = sum_{i=1}^n B_i, B_i iid Bernoulli(p)
#   E(B_i) = p, Var(B_i) = p(1-p)
#   => E(X) = n*p, Var(X) = n*p*(1-p)
mu <- n * p
sigma2 <- n * p * (1 - p)

hist(binom_var, freq = FALSE, breaks = 20,
     main = "Bin(100, 0.75) vs. normal approximation",
     xlab = "x")
curve(dnorm(x, mean = mu, sd = sqrt(sigma2)), add = TRUE, lwd = 2)

# ---- (b) ----
CLT_sim_binomial <- function(n, p, r) {

  # sum of n Bernoulli(p) draws, repeated r times (equiv. to Binomial(n, p))
  samples <- rbinom(r, size = n, prob = p)

  # standardize: E(sum) = n*p, Var(sum) = n*p*(1-p)
  z <- (samples - n * p) / sqrt(n * p * (1 - p))

  # fixed bin width so histograms stay comparable across n and p
  hist(z, freq = FALSE, xlim = c(-5, 5), breaks = seq(-50, 50, by = 0.25),
       main = paste0("n = ", n, ", p = ", p),
       xlab = "z")
  curve(dnorm(x), add = TRUE, lwd = 2)
}

CLT_sim_binomial(n = 100, p = 0.75, r = 1000)

# Convergence is fastest for p near 0.5 and slower as p approaches 0 or 1,
# where larger n is needed before the histogram matches the standard normal.

# ---- (c) ----
CLT_sim_exp <- function(n, lambda, r) {

  # r sample means, each from n iid draws of Exp(lambda)
  sample_means <- replicate(r, mean(rexp(n, lambda)))

  # standardize: E(mean) = 1/lambda, Var(mean) = 1/(n*lambda^2)
  x_mean <- 1 / lambda
  x_sd <- sqrt(1 / (n * lambda ^ 2))
  z <- (sample_means - x_mean) / x_sd

  hist(z, freq = FALSE, xlim = c(-5, 5), breaks = seq(-50, 50, by = 0.25),
       main = paste0("n = ", n, ", lambda = ", lambda),
       xlab = "z")
  curve(dnorm(x), add = TRUE, lwd = 2)
}

CLT_sim_exp(n = 5, lambda = 1, r = 1000)

# Convergence depends only on n, not on lambda, since Exp is a scale family
# and its skewness is the same for every lambda. For small n the right skew
# is still clearly visible; from n around 30 the fit is close.