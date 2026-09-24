# statistical inference 2
# tutorial sheet 2 exercise 2.1
# Topics: MCMC, Metropolis-Hastings, burn-in, autocorrelation, thinning

set.seed(1)

# Common settings
n <- 10000
proposal_sd <- 5

# ---- (a) ----
# Inputs needed for Metropolis-Hastings:
# - a starting value
# - a proposal distribution
# - a target function proportional to the desired density
# - the number of MCMC iterations

# ---- (b) ----
# The target is the same unnormalized mixture as in tutorial sheet 1.
# There are 5% more females than males.

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

# Random-walk proposal:
# given the current state y_t, propose
#
#   y_star ~ N(y_t, proposal_sd^2).
#
# The proposal is symmetric, so the proposal-density terms cancel in the
# Metropolis-Hastings acceptance probability.

proposal <- function(current, sd) {

  rnorm(
    1,
    mean = current,
    sd = sd
  )
}

# ---- (c) ----
# Metropolis-Hastings sampler.
#
# For a symmetric proposal, the acceptance probability is
#
#   alpha = min(1, target(y_star) / target(y_t)).
#
# If a proposal is rejected, the chain stays at the current value.

mcmc_sampler <- function(
    n_samples,
    starting_value,
    proposal_sd,
    target_fn
) {

  samples <- numeric(n_samples)
  current <- starting_value
  n_accepted <- 0

  for (i in seq_len(n_samples)) {

    proposed <- proposal(
      current,
      sd = proposal_sd
    )

    acceptance_ratio <- target_fn(proposed) /
      target_fn(current)

    acceptance_probability <- min(
      1,
      acceptance_ratio
    )

    if (runif(1) <= acceptance_probability) {

      current <- proposed
      n_accepted <- n_accepted + 1
    }

    samples[i] <- current
  }

  list(
    samples = samples,
    acceptance_rate = n_accepted / n_samples
  )
}

# Run the chain.
result_mcmc <- mcmc_sampler(
  n_samples = n,
  starting_value = 170,
  proposal_sd = proposal_sd,
  target_fn = target
)

# Acceptance rate
result_mcmc$acceptance_rate

# Histogram of MCMC draws.
hist(
  result_mcmc$samples,
  breaks = 50,
  freq = FALSE,
  xlab = "Body height",
  main = "MCMC sampling"
)

# Normalized target density for comparison with the histogram.
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

curve(
  target_density,
  add = TRUE,
  lwd = 2,
  lty = 2
)

# Inspect the first part of the Markov chain.
plot(
  result_mcmc$samples[1:100],
  type = "l",
  xlab = "Iteration",
  ylab = "Body height",
  main = "First 100 MCMC draws"
)

# ---- (d) ----
# Burn-in:
# if the starting value is far away from the high-density region, the chain
# needs time to move towards its stationary distribution.

result_burn_in <- mcmc_sampler(
  n_samples = n,
  starting_value = 0,
  proposal_sd = 5,
  target_fn = target
)

plot(
  result_burn_in$samples,
  type = "l",
  xlab = "Iteration",
  ylab = "Body height",
  main = "Burn-in from a poor starting value"
)

# Autocorrelation and proposal scale:
# small proposal steps are accepted often, but consecutive draws can be
# strongly correlated.

result_small_steps <- mcmc_sampler(
  n_samples = n,
  starting_value = 150,
  proposal_sd = 5,
  target_fn = target
)

result_small_steps$acceptance_rate

acf(
  result_small_steps$samples,
  main = "ACF with smaller proposal steps"
)

# Large proposal steps explore farther, but many proposals can be rejected.

result_large_steps <- mcmc_sampler(
  n_samples = n,
  starting_value = 170,
  proposal_sd = 50,
  target_fn = target
)

result_large_steps$acceptance_rate

acf(
  result_large_steps$samples,
  main = "ACF with larger proposal steps"
)

# ---- (e) ----
# Use a longer run and inspect the chain before post-processing.

n_final <- 1000000

result_final <- mcmc_sampler(
  n_samples = n_final,
  starting_value = 170,
  proposal_sd = 45,
  target_fn = target
)

result_final$acceptance_rate

acf(
  result_final$samples,
  main = "ACF before thinning"
)

# Following the suggested solution, retain every tenth draw.
thin_by <- 10

samples_thinned <- result_final$samples[
  seq(
    1,
    length(result_final$samples),
    by = thin_by
  )
]

hist(
  samples_thinned,
  breaks = 50,
  freq = FALSE,
  xlab = "Body height",
  main = "Thinned MCMC sample"
)

curve(
  target_density,
  add = TRUE,
  lwd = 2,
  lty = 2
)
