# statistical inference 1
# sheet 11 exercise 11.1
# Topics: one-sided t-test, Monte Carlo simulation,
#         simulated power, theoretical power

set.seed(1)


# ---- setup ----
mu0 <- 0
sigma <- 1
alpha <- 0.05
n <- 50
r <- 5000

delta_grid <- seq(-1, 0.5, by = 0.05)


# ---- (a) ----
perform_test <- function(sample_size, mu0, sigma, delta, alpha) {

  # true mean under the chosen deviation delta
  mu <- mu0 + delta

  # simulate one sample
  x <- rnorm(sample_size, mean = mu, sd = sigma)

  # one-sided t-test:
  # H0: mu >= mu0  vs.  H1: mu < mu0
  test <- t.test(
    x,
    alternative = "less",
    mu = mu0,
    conf.level = 1 - alpha
  )

  # reject if the test statistic lies in the lower critical region
  critical_value <- qt(alpha, df = sample_size - 1)

  unname(test$statistic) < critical_value
}


# ---- (b) ----
simulation_based_power <- function(sim_size, sample_size, mu0,
                                   sigma, delta, alpha) {

  rejected <- replicate(
    sim_size,
    perform_test(
      sample_size = sample_size,
      mu0 = mu0,
      sigma = sigma,
      delta = delta,
      alpha = alpha
    )
  )

  mean(rejected)
}


# Example: simulated power at delta = -0.3
simulation_based_power(
  sim_size = r,
  sample_size = n,
  mu0 = mu0,
  sigma = sigma,
  delta = -0.3,
  alpha = alpha
)


# ---- (c) ----
# simulated power for a grid of delta values
simulated_power <- sapply(
  delta_grid,
  function(delta) {
    simulation_based_power(
      sim_size = r,
      sample_size = n,
      mu0 = mu0,
      sigma = sigma,
      delta = delta,
      alpha = alpha
    )
  }
)


# theoretical power using the non-central t distribution
theoretical_power <- sapply(
  delta_grid,
  function(delta) {

    critical_value <- qt(alpha, df = n - 1)
    ncp <- delta * sqrt(n) / sigma

    pt(
      critical_value,
      df = n - 1,
      ncp = ncp
    )
  }
)


# simulated and theoretical power curve
plot(delta_grid,
     simulated_power,
     type = "l",
     lwd = 2,
     ylim = c(0, 1),
     xlab = expression(delta),
     ylab = "Power",
     main = "Power of a one-sided t-test")

lines(delta_grid,
      theoretical_power,
      lwd = 2,
      lty = 2)

abline(h = alpha, lty = 3)
abline(v = 0, lty = 3)

legend(
  "topright",
  legend = c(
    "simulated power",
    "theoretical power",
    expression(alpha)
  ),
  lty = c(1, 2, 3),
  lwd = c(2, 2, 1),
  bty = "n"
)


# ---- effect of sample size ----
sample_sizes <- c(10, 25, 50, 100)

power_by_n <- sapply(
  sample_sizes,
  function(n_i) {

    sapply(
      delta_grid,
      function(delta) {

        critical_value <- qt(alpha, df = n_i - 1)
        ncp <- delta * sqrt(n_i) / sigma

        pt(
          critical_value,
          df = n_i - 1,
          ncp = ncp
        )
      }
    )
  }
)

matplot(delta_grid,
        power_by_n,
        type = "l",
        lty = 1:4,
        lwd = 2,
        ylim = c(0, 1),
        xlab = expression(delta),
        ylab = "Power",
        main = "Effect of sample size on power")

abline(h = alpha, lty = 3)
abline(v = 0, lty = 3)

legend(
  "topright",
  legend = paste("n =", sample_sizes),
  lty = 1:4,
  lwd = 2,
  bty = "n"
)
