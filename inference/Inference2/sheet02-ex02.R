# statistical inference 2
# sheet 2 exercise 2.2
# Topics: MCMC, Metropolis-Hastings, autocorrelation, thinning

# Sample from the conditional distribution of two independent Exp(1)
# variables given that X1 + X2 > 10. First, quantify how rare this event
# is, then use Metropolis-Hastings to sample efficiently from the target
# distribution and inspect the dependence within the resulting Markov chain.


set.seed(1)


# Common settings
n <- 1000
threshold <- 10
proposal_sd <- 1



# ---- (a) ----
# Since X1, X2 ~ Exp(1) independently,
# S = X1 + X2 follows a Gamma(shape = 2, rate = 1) distribution.
# The tail probability shows how inefficient naive rejection sampling would be.

tail_probability <- pgamma(
  threshold,
  shape = 2,
  rate = 1,
  lower.tail = FALSE
)

tail_probability

# P(S > 10) is very small, so brute-force rejection is inefficient.
# Increasing the threshold makes the tail probability even smaller.



# ---- (b) ----
# Use Metropolis-Hastings to sample from
# (X1, X2) | X1 + X2 > 10.
#
# The chain starts inside the admissible region and proposes nearby points
# using a Gaussian random walk. Proposals outside the target region have
# target density 0 and are therefore rejected.


# Log target density, up to a normalizing constant.
log_target <- function(x, threshold) {

  if (any(x <= 0) || sum(x) <= threshold) {
    return(-Inf)
  }

  sum(dexp(x, rate = 1, log = TRUE))
}


mh_sampler <- function(n_samples, start, proposal_sd, threshold) {

  samples <- matrix(
    NA_real_,
    nrow = n_samples,
    ncol = 2
  )

  n_accepted <- 0
  x <- start

  for (i in 1:n_samples) {

    # Gaussian random-walk proposal
    proposal <- rnorm(
      2,
      mean = x,
      sd = proposal_sd
    )

    # The proposal is symmetric, so the q-terms cancel.
    alpha <- min(
      1,
      exp(
        log_target(proposal, threshold) -
          log_target(x, threshold)
      )
    )

    if (runif(1) <= alpha) {
      x <- proposal
      n_accepted <- n_accepted + 1
    }

    samples[i, ] <- x
  }

  list(
    samples = samples,
    acceptance_rate = n_accepted / n_samples
  )
}


# Start inside the admissible region.
start <- c(5.01, 5.01)

result_mh <- mh_sampler(
  n_samples = n,
  start = start,
  proposal_sd = proposal_sd,
  threshold = threshold
)


# Acceptance rate
result_mh$acceptance_rate


# Sampling path
plot(
  result_mh$samples[, 1],
  result_mh$samples[, 2],
  type = "l",
  xlab = "X1",
  ylab = "X2",
  main = "Metropolis-Hastings sampling path"
)

abline(
  a = threshold,
  b = -1,
  lty = 2
)



# ---- (c) ----
# Consecutive MCMC draws are generally dependent because each state is
# generated from the previous one. The autocorrelation function measures
# how strongly S_t = X1_t + X2_t is related to earlier values of the chain.

s <- rowSums(result_mh$samples)

acf(
  s,
  main = "Autocorrelation of S"
)


# Numerical ACF values
acf_result <- acf(
  s,
  plot = FALSE,
  lag.max = 100
)

acf_values <- as.numeric(acf_result$acf)


# Use the first lag with |ACF| < 0.05 as a simple thinning rule.
thin_lag <- which(
  abs(acf_values[-1]) < 0.05
)[1]

thin_lag


# Thin the chain to reduce dependence between retained draws.
samples_thinned <- result_mh$samples[
  seq(1, nrow(result_mh$samples), by = thin_lag),
  ,
  drop = FALSE
]


# Check the remaining autocorrelation.
acf(
  rowSums(samples_thinned),
  main = "Autocorrelation after thinning"
)
