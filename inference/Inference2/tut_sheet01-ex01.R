# statistical inference 2
# sheet 1 exercise 1.1
# Topics: rejection sampling, proposal distribution, mixture density


set.seed(1)


# Common settings
n <- 100000
alpha <- 3


# ---- (a) ----
# The target is only known up to a normalizing constant.
# There are 5% more females than males, so the female component receives
# weight 1.05 and the male component weight 1.

target <- function(x) {

  female <- dnorm(
    x,
    mean = 165,
    sd = 7
  )

  male <- dnorm(
    x,
    mean = 180,
    sd = 7
  )

  1.05 * female + male
}


# Use a Gaussian proposal that is easy to sample from and has wider tails
# than the target mixture.

proposal <- function(x) {

  dnorm(
    x,
    mean = 172.5,
    sd = 15
  )
}


# The scaled proposal alpha * q(x) must dominate the target everywhere.
# The suggested solution uses alpha = 3.

scaled_proposal <- function(x) {

  alpha * proposal(x)
}


# Compare target, proposal, and scaled proposal.
curve(
  target,
  from = 140,
  to = 210,
  ylim = c(0, 0.1),
  lwd = 2,
  xlab = "Body height",
  ylab = "Density",
  main = "Target and proposal"
)

curve(
  proposal,
  add = TRUE,
  lty = 2
)

curve(
  scaled_proposal,
  add = TRUE,
  lty = 3
)

legend(
  "topright",
  legend = c(
    "Target",
    "Proposal q(x)",
    "Scaled proposal alpha q(x)"
  ),
  lwd = c(2, 1, 1),
  lty = c(1, 2, 3)
)



# ---- (b) ----
# Rejection sampling:
# 1. Draw Y* from q.
# 2. Draw U ~ Uniform(0, 1).
# 3. Accept Y* if
#       U <= f(Y*) / (alpha q(Y*)).

rejection_sampling <- function(n_samples) {

  samples <- numeric(n_samples)

  i <- 0
  n_proposals <- 0

  while (i < n_samples) {

    y_star <- rnorm(
      1,
      mean = 172.5,
      sd = 15
    )

    acceptance_probability <- target(y_star) /
      scaled_proposal(y_star)

    if (runif(1) <= acceptance_probability) {

      i <- i + 1
      samples[i] <- y_star
    }

    n_proposals <- n_proposals + 1
  }

  list(
    samples = samples,
    acceptance_rate = n_samples / n_proposals
  )
}


result_rejection <- rejection_sampling(n)


# Acceptance rate
result_rejection$acceptance_rate


# Histogram of accepted draws.
hist(
  result_rejection$samples,
  breaks = 50,
  freq = FALSE,
  xlab = "Body height",
  main = "Rejection sampling",
  ylim = c(0, 0.05)
)


# Kernel density estimate based on the simulated sample.
density_estimate <- density(
  result_rejection$samples
)

lines(
  density_estimate$x,
  density_estimate$y,
  lwd = 2
)



# ---- (c) ----
# Normalize the mixture analytically.
# If there are 5% more females than males, the population weights are
#
#   female: 1.05 / 2.05
#   male:   1.00 / 2.05

female_weight <- 1.05 / 2.05
male_weight <- 1 / 2.05


target_density <- function(x) {

  female_weight * dnorm(
    x,
    mean = 165,
    sd = 7
  ) +
    male_weight * dnorm(
      x,
      mean = 180,
      sd = 7
    )
}


# Add the analytical target density to the histogram.
curve(
  target_density,
  add = TRUE,
  lty = 2,
  lwd = 2
)

legend(
  "topright",
  legend = c(
    "Estimated density from samples",
    "Analytical target density"
  ),
  lwd = 2,
  lty = c(1, 2)
)


# Since the target is a known two-component mixture here, direct mixture
# sampling is possible and more efficient than rejection sampling.

sample_from_mixture <- function(n_samples) {

  is_female <- runif(n_samples) < female_weight

  samples <- numeric(n_samples)

  samples[is_female] <- rnorm(
    sum(is_female),
    mean = 165,
    sd = 7
  )

  samples[!is_female] <- rnorm(
    sum(!is_female),
    mean = 180,
    sd = 7
  )

  samples
}


samples_mixture <- sample_from_mixture(n)
