# statistical inference 1
# sheet 03 exercise 3.3
# Topics: simulation study, empirical vs. analytical variance,
#         bias, MSE, Monte Carlo error

# T_n is an unbiased estimator for sigma^2 based on an i.i.d. sample from
# N(0, sigma^2), weighting the first observation differently from the rest.
# We simulate many samples per sample size n, compute T_n for each, and
# read off empirical variance, bias and MSE (d). We then compare the
# empirical variance with the analytical result Var(T_n) = 2 sigma^4 / (n - 1)
# derived in (b), see (e).

set.seed(42)

n_sim <- 1000     # Monte Carlo replications per sample size
mu <- 0           # the estimator assumes a known mean of 0
sigma2 <- 2       # true parameter that T_n estimates

# ---- (d) ----
# T_n = 2/n x_1^2 + (n - 2) / (n (n - 1)) sum_{i >= 2} x_i^2.
# x[-1] drops the first element, so sum(x[-1]^2) is the sum from i = 2 to n.
t_n <- function(x) {
  n <- length(x)
  (2 / n) * x[1]^2 + (n - 2) / (n * (n - 1)) * sum(x[-1]^2)
}

# Empirical variance, bias and MSE of T_n for one sample size.
sim_metrics <- function(n, mu, sigma2, n_sim) {
  t_values <- replicate(n_sim, t_n(rnorm(n, mean = mu, sd = sqrt(sigma2))))

  c(variance = var(t_values),
    bias = mean(t_values) - sigma2,
    mse = mean((t_values - sigma2)^2))
}

n_values <- seq(10, 1000, by = 5)
results <- sapply(n_values, sim_metrics, mu = mu, sigma2 = sigma2,
                  n_sim = n_sim)

plot_metrics <- function(n_values, results) {
  old_par <- par(mfrow = c(3, 1))
  on.exit(par(old_par))

  plot(n_values, results["variance", ], type = "l", col = "blue", lwd = 2,
       xlab = "n", ylab = "empirical variance",
       main = "Empirical variance of T_n")
  plot(n_values, results["bias", ], type = "l", col = "red", lwd = 2,
       xlab = "n", ylab = "bias", main = "Bias of T_n")
  abline(h = 0, lty = 2)
  plot(n_values, results["mse", ], type = "l", col = "darkgreen", lwd = 2,
       xlab = "n", ylab = "MSE", main = "MSE of T_n")
}

plot_metrics(n_values, results)

# The variance decays to 0 in n, which together with unbiasedness gives
# MSE consistency; the MSE curve is indistinguishable from the variance
# curve because the squared bias is orders of magnitude smaller.
# The bias fluctuates around 0 rather than sitting on it: that is Monte
# Carlo error from averaging over finitely many replications, not a
# contradiction to the unbiasedness shown in (a).

# ---- (e) ----
# Analytical variance from (b): Var(T_n) = 2 sigma^4 / (n - 1),
# here 8 / (n - 1) for sigma2 = 2.
theo_var <- function(n, sigma2) {
  2 * sigma2^2 / (n - 1)
}

theo_values <- theo_var(n_values, sigma2 = sigma2)

plot(n_values, results["variance", ], type = "l", col = "blue", lwd = 2,
     xlab = "n", ylab = "variance",
     ylim = range(c(results["variance", ], theo_values)),
     main = "Empirical vs. analytical variance of T_n")
lines(n_values, theo_values, col = "red", lty = 2, lwd = 2)
legend("topright", bty = "n", col = c("blue", "red"),
       lty = c(1, 2), lwd = 2,
       legend = c("empirical", expression(2 * sigma^4 / (n - 1))))

# Numerical comparison at three sample sizes; the empirical column depends
# on the seed, the analytical one does not.
check_at <- c(10, 100, 1000)
comparison <- data.frame(
  n = check_at,
  empirical = round(results["variance", match(check_at, n_values)], 4),
  analytical = round(theo_var(check_at, sigma2 = sigma2), 4)
)
print(comparison, row.names = FALSE)

# The two curves lie on top of each other over the whole range, so the
# simulation confirms the derivation in (b). Visible gaps occur only for
# small n, where Var(T_n) is large and 1000 replications estimate it less
# precisely; that is Monte Carlo error, not a flaw in the formula.