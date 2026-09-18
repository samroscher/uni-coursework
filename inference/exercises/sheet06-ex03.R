# statistical inference 1
# sheet 06 exercise 6.3
# Topics: misspecified MLE, sandwich variance,
#         Monte Carlo simulation, empirical variance

# We generate data from a Gamma distribution but fit an Exponential model.
# The MLE for the Exponential mean is still the sample mean. We compare the
# naive model-based variance with the sandwich variance across many samples.

set.seed(1)

# ---- setup ----
n <- 100
r <- 10000

# true DGP moments
theta0 <- 5
c_true <- 10

# choose Gamma(shape, rate) so that
# E(X) = theta0 and Var(X) = c_true
shape <- theta0^2 / c_true
rate <- theta0 / c_true

# theoretical variance of the MLE under the true DGP
true_var <- c_true / n

# variance implied by the incorrectly assumed Exponential model
model_based_var <- theta0^2 / n


# ---- score and Hessian contributions ----
score <- function(x, theta) {
  -1 / theta + x / theta^2
}

neg_hessian <- function(x, theta) {
  -1 / theta^2 + 2 * x / theta^3
}


# ---- (d) ----
simulate_once <- function(n, shape, rate) {

  # true data come from Gamma, not Exponential
  x <- rgamma(n, shape = shape, rate = rate)

  # MLE under the assumed Exponential model
  theta_hat <- mean(x)

  # naive variance: what the Exponential model reports if it were correct
  var_naive <- theta_hat^2 / n

  # empirical bread J_hat and meat I_hat
  J_hat <- mean(neg_hessian(x, theta_hat))
  I_hat <- mean(score(x, theta_hat)^2)

  # sandwich variance of theta_hat
  var_sandwich <- (1 / n) * (1 / J_hat) * I_hat * (1 / J_hat)

  c(theta_hat = theta_hat,
    var_naive = var_naive,
    var_sandwich = var_sandwich)
}

sim <- t(replicate(
  r,
  simulate_once(n = n, shape = shape, rate = rate)
))

sim <- as.data.frame(sim)


# ---- (e) ----
# empirical sampling variance of the MLE across all repetitions
empirical_var <- var(sim$theta_hat)

# average estimated variances
avg_naive <- mean(sim$var_naive)
avg_sandwich <- mean(sim$var_sandwich)

cat("True asymptotic variance:       ", round(true_var, 4), "\n")
cat("Naive model variance target:    ", round(model_based_var, 4), "\n")
cat("Empirical variance of the MLE:  ", round(empirical_var, 4), "\n")
cat("Average naive estimate:         ", round(avg_naive, 4), "\n")
cat("Average sandwich estimate:      ", round(avg_sandwich, 4), "\n")


# Sampling distribution of the MLE
hist(sim$theta_hat,
     freq = FALSE,
     breaks = 40,
     main = "Sampling distribution of the MLE",
     xlab = expression(hat(theta)),
     ylab = "Density",
     xlim = c(3.8, 6.5))

curve(dnorm(x, mean = theta0, sd = sqrt(true_var)),
      add = TRUE,
      lwd = 2)

curve(dnorm(x, mean = theta0, sd = sqrt(model_based_var)),
      add = TRUE,
      lwd = 2,
      lty = 2)

abline(v = theta0, lty = 3)

legend(
  "topright",
  inset = 0.02,
  legend = c(
    "true / sandwich",
    "naive model",
    "true mean"
  ),
  lty = c(1, 2, 3),
  lwd = c(2, 2, 1),
  seg.len = 1.2,
  x.intersp = 0.7,
  cex = 0.9,
  bty = "n"
)


# Estimated variance across repetitions
boxplot(sim$var_naive,
        sim$var_sandwich,
        names = c("Naive", "Sandwich"),
        main = "Estimated variance across simulations",
        ylab = "Estimated variance",
        outline = FALSE)

abline(h = empirical_var, lty = 2, lwd = 2)
abline(h = true_var, lty = 3, lwd = 2)

legend("topright",
       legend = c("Empirical variance", "True variance"),
       lty = c(2, 3),
       lwd = c(2, 2),
       bty = "n")
