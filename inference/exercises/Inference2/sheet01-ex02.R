# statistical inference 2
# sheet 1 exercise 1.2
# Topics: rejection sampling, importance sampling,
#         Monte Carlo integration, standard errors


set.seed(1)


# ---- (a) ----
# Implement rejection sampling for f(x) ∝ exp(-x^2) on [-a, a]
# using Uniform(-a, a) as proposal.

rejection_sampler <- function(n_samples, a) {

  samples <- numeric(n_samples)
  n_accepted <- 0
  n_proposals <- 0

  while (n_accepted < n_samples) {

    # proposal draw Y ~ Uniform(-a, a)
    y <- runif(1, -a, a)

    # since M = 2a, the acceptance probability simplifies to exp(-y^2)
    accept <- runif(1) <= exp(-y^2)

    n_proposals <- n_proposals + 1

    if (accept) {
      n_accepted <- n_accepted + 1
      samples[n_accepted] <- y
    }
  }

  list(
    samples = samples,
    acceptance_rate = n_samples / n_proposals
  )
}



# ---- (b) ----
# Draw n = 1000 samples for a = 0.1, 1, and 10
# and compare their empirical densities.

n <- 1000

result_01 <- rejection_sampler(n, a = 0.1)
result_1  <- rejection_sampler(n, a = 1)
result_10 <- rejection_sampler(n, a = 10)


# Acceptance rates
c(
  a_0.1 = result_01$acceptance_rate,
  a_1   = result_1$acceptance_rate,
  a_10  = result_10$acceptance_rate
)


# Empirical densities
par(mfrow = c(3, 1))

plot(
  density(result_01$samples),
  main = "Density of samples (a = 0.1)",
  xlab = "x",
  ylab = "Density"
)

plot(
  density(result_1$samples),
  main = "Density of samples (a = 1)",
  xlab = "x",
  ylab = "Density"
)

plot(
  density(result_10$samples),
  main = "Density of samples (a = 10)",
  xlab = "x",
  ylab = "Density"
)

par(mfrow = c(1, 1))


# As a increases, the bell shape becomes more visible,
# while the rejection sampler becomes less efficient.



# ---- (c) ----
# Use importance sampling to estimate
# I_a = integral from -a to a of cos(x) / (1 + x^2).

h <- function(x) {
  cos(x) / (1 + x^2)
}


importance_sampler <- function(n_samples, a) {

  # draw X_i from q(x) ∝ exp(-x^2) using part (a)
  draw <- rejection_sampler(n_samples, a)
  x <- draw$samples

  # q must be normalized for importance sampling
  C <- integrate(
    function(x) exp(-x^2),
    lower = -a,
    upper = a
  )$value

  q <- function(x) {
    exp(-x^2) / C
  }

  # importance contributions Z_i = h(X_i) / q(X_i)
  z <- h(x) / q(x)

  list(
    estimate = mean(z),
    standard_error = sd(z) / sqrt(n_samples)
  )
}


# Example: estimate I_a for a = 1
a <- 1

result <- importance_sampler(
  n_samples = n,
  a = a
)

result$estimate


# Numerical value for comparison
true_value <- integrate(
  h,
  lower = -a,
  upper = a
)$value

true_value



# ---- (d) ----
# Compute the estimated standard error of the importance-sampling estimator.
#
# With Z_i = h(X_i) / q(X_i),
# I_hat is the sample mean of the Z_i, so
# SE_hat = sd(Z_i) / sqrt(n).

result$standard_error
