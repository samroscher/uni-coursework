# statistical inference 2
# sheet 5 exercise 5.1
# Topic: Approximate Bayesian Computation (ABC)

# Estimate parameters of a geometric Brownian motion for a DAX segment
# by simulating from the prior and retaining parameter values that produce
# paths close to the observed data.


set.seed(42)

data(EuStockMarkets)


# Observed DAX closing prices
y <- EuStockMarkets[625:875, 1]


# Common settings
epsilon <- 6500
n_posterior <- 200
max_trials <- 500000



# ---- (a) ----
# Implement ABC for mu, sigma and dt.


# Draw one parameter vector from the prior.
simulate_prior <- function() {

  mu_star <- rnorm(
    1,
    mean = 0,
    sd = 1
  )

  sigma_star <- runif(
    1,
    min = 0,
    max = 1
  )

  dt_star <- runif(
    1,
    min = 0.001,
    max = 0.01
  )

  c(
    mu = mu_star,
    sigma = sigma_star,
    dt = dt_star
  )
}


# Simulate one geometric Brownian motion path.
simulate_data <- function(
    n,
    mu,
    sigma,
    dt,
    first_value
) {

  increments <- rnorm(
    n,
    mean = (mu - sigma^2 / 2) * dt,
    sd = sigma * sqrt(dt)
  )

  log_path <- cumsum(increments)

  first_value * exp(log_path)
}


# Mean squared distance between observed and simulated data.
compute_distance <- function(
    data,
    simulated_data
) {

  mean(
    (data - simulated_data)^2
  )
}


# Rejection ABC:
# 1. draw parameters from the prior
# 2. simulate a path
# 3. accept the parameters if the distance is at most epsilon
run_abc <- function(
    n,
    y,
    epsilon,
    max_trials = 500000
) {

  posterior <- data.frame(
    mu = numeric(0),
    sigma = numeric(0),
    dt = numeric(0)
  )

  n_trials <- 0


  while (
    nrow(posterior) < n &&
    n_trials < max_trials
  ) {

    n_trials <- n_trials + 1


    # Step 1: sample from the prior
    prior <- simulate_prior()


    # Step 2: simulate data with the sampled parameters
    y_star <- simulate_data(
      n = length(y),
      mu = prior["mu"],
      sigma = prior["sigma"],
      dt = prior["dt"],
      first_value = y[1]
    )


    # Step 3: compare simulated and observed data
    distance <- compute_distance(
      data = y,
      simulated_data = y_star
    )


    # Accept if the simulated path is sufficiently close
    if (distance <= epsilon) {

      posterior <- rbind(
        posterior,
        data.frame(
          mu = prior["mu"],
          sigma = prior["sigma"],
          dt = prior["dt"]
        )
      )
    }


    if (n_trials %% 10000 == 0) {

      cat(
        "Trials:", n_trials,
        "- accepted:", nrow(posterior),
        "\n"
      )
    }
  }


  cat(
    "Generated",
    nrow(posterior),
    "posterior samples after",
    n_trials,
    "trials.\n"
  )

  posterior
}


posterior <- run_abc(
  n = n_posterior,
  y = y,
  epsilon = epsilon,
  max_trials = max_trials
)


# Inspect the accepted parameter values.
plot(
  posterior,
  main = "ABC posterior samples"
)



# ---- (b) ----
# Check whether mu = 0 is plausible under the posterior.


mu_values <- posterior$mu

posterior_mean_mu <- mean(mu_values)

credible_interval_mu <- quantile(
  mu_values,
  probs = c(0.025, 0.975)
)


posterior_mean_mu
credible_interval_mu


hist(
  mu_values,
  breaks = 30,
  main = "Posterior distribution of mu",
  xlab = expression(mu)
)

abline(
  v = posterior_mean_mu,
  lwd = 2,
  lty = 2
)

abline(
  v = credible_interval_mu,
  lwd = 2,
  lty = 3
)


# Interpretation:
# If 0 lies inside the 95% credible interval, the posterior is compatible
# with a driftless process (mu = 0).



# ---- (c) ----
# Compare the observed DAX path with simulations based on posterior means.


visualize_posterior <- function(
    posterior,
    y,
    n_paths = 15,
    ylim = NULL
) {

  post_mu <- mean(posterior$mu)
  post_sigma <- mean(posterior$sigma)
  post_dt <- mean(posterior$dt)


  plot(
    y,
    type = "l",
    lwd = 3,
    ylim = ylim,
    xlab = "Time",
    ylab = "True / simulated DAX stock index",
    main = "Observed data and posterior simulations"
  )


  for (i in seq_len(n_paths)) {

    simulated <- simulate_data(
      n = length(y),
      mu = post_mu,
      sigma = post_sigma,
      dt = post_dt,
      first_value = y[1]
    )

    lines(
      simulated,
      col = rgb(0, 0, 1, 0.35)
    )
  }
}


visualize_posterior(
  posterior = posterior,
  y = y
)


# The posterior simulations can reproduce similar stochastic behaviour,
# but they do not reproduce the exact historical DAX path.



# Compare with paths generated directly from random prior draws.
plot(
  y,
  type = "l",
  lwd = 3,
  ylim = c(0, 4000),
  xlab = "Time",
  ylab = "True / simulated DAX stock index",
  main = "Posterior vs prior simulations"
)


# Posterior-based paths
for (i in 1:15) {

  simulated <- simulate_data(
    n = length(y),
    mu = mean(posterior$mu),
    sigma = mean(posterior$sigma),
    dt = mean(posterior$dt),
    first_value = y[1]
  )

  lines(
    simulated,
    col = rgb(0, 0, 1, 0.25)
  )
}


# Prior-based paths
for (i in 1:15) {

  prior <- simulate_prior()

  simulated <- simulate_data(
    n = length(y),
    mu = prior["mu"],
    sigma = prior["sigma"],
    dt = prior["dt"],
    first_value = y[1]
  )

  lines(
    simulated,
    col = rgb(1, 0, 0, 0.25)
  )
}



# ---- (d) ----
# Study the effect of changing epsilon.


# Smaller epsilon:
# fewer proposals are accepted and the approximation is stricter,
# but computation becomes more expensive.
epsilon_small <- 5000

posterior_small <- run_abc(
  n = 50,
  y = y,
  epsilon = epsilon_small,
  max_trials = max_trials
)


# Larger epsilon:
# more proposals are accepted, but the posterior becomes less informative.
epsilon_large <- 50000

posterior_large <- run_abc(
  n = n_posterior,
  y = y,
  epsilon = epsilon_large,
  max_trials = max_trials
)


# In the limit epsilon -> infinity, every prior draw is accepted.
# Therefore, the ABC posterior approaches the prior.
