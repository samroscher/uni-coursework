# statistical inference 2
# sheet 1 exercise 1.2
# Topics: importance sampling, self-normalized weights, effective sample size


set.seed(1)


# Common settings
n <- 10000


# ---- (a) ----
# The target density is p(x) = f(x) / C, where C is unknown.
# Since
#
#   mu = E_p[X]
#      = integral x f(x) dx / integral f(x) dx,
#
# we can use importance sampling with any proposal density q satisfying
# q(x) > 0 whenever f(x) > 0.

f <- function(x) {

  density <- 0.2 * x * exp(-x / 3) *
    (1 + 0.2 * cos(2 * x))

  density * (x > 0)
}


# Proposal densities from the exercise.
q1 <- function(x) {

  dgamma(
    x,
    shape = 2,
    scale = 3
  )
}


q2 <- function(x) {

  dgamma(
    x,
    shape = 5,
    scale = 3
  )
}


# For X_i ~ q, define importance weights
#
#   w_i = f(X_i) / q(X_i).
#
# The self-normalized importance sampling estimator is
#
#   mu_hat = sum X_i w_i / sum w_i.
#
# The unknown normalizing constant C is not needed because it cancels
# between numerator and denominator.



# ---- (b) ----
# First compare the shape of the unnormalized target f with both proposals.

x_values <- seq(
  0,
  25,
  length.out = 1000
)

plot(
  x_values,
  f(x_values),
  type = "l",
  lwd = 2,
  xlab = "x",
  ylab = "Density",
  main = "Target and proposal densities"
)

curve(
  q1,
  add = TRUE,
  lty = 2
)

curve(
  q2,
  add = TRUE,
  lty = 3
)

legend(
  "topright",
  legend = c(
    "f(x)",
    "q1(x) = Gamma(2, 3)",
    "q2(x) = Gamma(5, 3)"
  ),
  lwd = c(2, 1, 1),
  lty = c(1, 2, 3)
)


# Importance sampling with q1.
x_q1 <- rgamma(
  n,
  shape = 2,
  scale = 3
)

weights_q1 <- f(x_q1) / q1(x_q1)

mu_est_q1 <- sum(
  x_q1 * weights_q1
) / sum(weights_q1)


# Importance sampling with q2.
x_q2 <- rgamma(
  n,
  shape = 5,
  scale = 3
)

weights_q2 <- f(x_q2) / q2(x_q2)

mu_est_q2 <- sum(
  x_q2 * weights_q2
) / sum(weights_q2)


# Estimated means
mu_est_q1
mu_est_q2



# ---- (c) ----
# Normalize the importance weights so that they sum to one.

weights_q1_normalized <- weights_q1 / sum(weights_q1)
weights_q2_normalized <- weights_q2 / sum(weights_q2)


# Effective sample size:
#
#   ESS = 1 / sum(normalized_weight_i^2).
#
# If all weights are similar, ESS is close to n.
# If only a few weights dominate, ESS is much smaller.

ess_q1 <- 1 / sum(
  weights_q1_normalized^2
)

ess_q2 <- 1 / sum(
  weights_q2_normalized^2
)


# Compare effective sample sizes.
ess_q1
ess_q2


# Inspect the raw importance weights.
par(
  mfrow = c(1, 2)
)

hist(
  weights_q1,
  freq = FALSE,
  main = "Weights under q1",
  xlab = "w"
)

hist(
  weights_q2,
  freq = FALSE,
  main = "Weights under q2",
  xlab = "w"
)

par(
  mfrow = c(1, 1)
)
