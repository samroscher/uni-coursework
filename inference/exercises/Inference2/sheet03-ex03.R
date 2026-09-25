# statistical inference 2
# sheet 3 exercise 3.3
# Topics: sampling functions, autocorrelation, burn-in, thinning

# Simulate plausible versions of an unknown function on a grid while keeping
# the observed function values fixed.


set.seed(123)


# Known function evaluations
x_known <- c(0.10, 0.12, 0.18, 0.43, 0.51, 0.68, 0.86, 0.95)
y_known <- c(0.3750, 0.3015, 0.0927, 0.2374,
             -0.5391, -0.0649, 0.3946, -0.4626)


# Common settings
a <- 0
b <- 1
m <- 100
J <- 15000
sigma <- sd(y_known)

burn_in <- 100
thinning <- 50



# ---- (a) ----
# Simulate J discretized functions on an equidistant grid.

simulate_f <- function(
    x_known,
    y_known,
    a = 0,
    b = 1,
    m = 100,
    J = 1000,
    sigma = sd(y_known),
    direction = c("forward", "reverse")
) {

  direction <- match.arg(direction)

  x_grid <- seq(a, b, length.out = m + 1)[-1]
  y_sim <- matrix(NA_real_, nrow = J, ncol = m)

  known_idx <- sapply(
    x_known,
    function(x) which.min(abs(x_grid - x))
  )

  # Initial function
  y_sim[1, ] <- rnorm(
    m,
    mean = mean(y_known),
    sd = sd(y_known)
  )

  y_sim[1, known_idx] <- y_known


  for (j in 2:J) {

    index_order <- if (direction == "forward") {
      1:m
    } else {
      m:1
    }


    for (i in index_order) {

      # Keep observed values fixed
      if (i %in% known_idx) {
        y_sim[j, i] <- y_known[match(i, known_idx)]
        next
      }


      if (direction == "forward") {

        if (i == 1) {
          mu <- y_sim[j - 1, i + 1]
        } else if (i == m) {
          mu <- y_sim[j, i - 1]
        } else {
          mu <- 0.5 * (
            y_sim[j, i - 1] +
              y_sim[j - 1, i + 1]
          )
        }

      } else {

        if (i == m) {
          mu <- y_sim[j - 1, i - 1]
        } else if (i == 1) {
          mu <- y_sim[j, i + 1]
        } else {
          mu <- 0.5 * (
            y_sim[j, i + 1] +
              y_sim[j - 1, i - 1]
          )
        }
      }


      y_sim[j, i] <- rnorm(
        1,
        mean = mu,
        sd = sigma
      )
    }
  }


  list(
    x = x_grid,
    y = y_sim
  )
}


result <- simulate_f(
  x_known = x_known,
  y_known = y_known,
  a = a,
  b = b,
  m = m,
  J = J,
  sigma = sigma
)


# Inspect autocorrelation at a few grid points.
acf(result$y[, 1], lag.max = 100)
acf(result$y[, 30], lag.max = 100)
acf(result$y[, 70], lag.max = 100)


# Remove burn-in and thin the chain.
samples <- result$y[-seq_len(burn_in), ]

samples_thinned <- samples[
  seq(1, nrow(samples), by = thinning),
  ,
  drop = FALSE
]

nrow(samples_thinned)



# ---- (b) ----
# Plot the retained simulated functions and the known points.

matplot(
  result$x,
  t(samples_thinned),
  type = "l",
  lty = 1,
  col = rgb(0, 0, 0, 0.05),
  xlab = "x",
  ylab = "f(x)",
  main = "Simulated functions"
)

points(
  x_known,
  y_known,
  pch = 19
)


# Pointwise uncertainty across the retained simulations.
pointwise_sd <- apply(
  samples_thinned,
  2,
  sd
)

ci_lower <- apply(
  samples_thinned,
  2,
  quantile,
  probs = 0.025
)

ci_upper <- apply(
  samples_thinned,
  2,
  quantile,
  probs = 0.975
)


lines(
  result$x,
  ci_lower,
  lwd = 2
)

lines(
  result$x,
  ci_upper,
  lwd = 2
)



# ---- (c) ----
# Repeat the simulation while updating grid points from right to left.

result_reverse <- simulate_f(
  x_known = x_known,
  y_known = y_known,
  a = a,
  b = b,
  m = m,
  J = J,
  sigma = sigma,
  direction = "reverse"
)


samples_reverse <- result_reverse$y[-seq_len(burn_in), ]

samples_reverse_thinned <- samples_reverse[
  seq(1, nrow(samples_reverse), by = thinning),
  ,
  drop = FALSE
]


matplot(
  result_reverse$x,
  t(samples_reverse_thinned),
  type = "l",
  lty = 1,
  col = rgb(0, 0, 0, 0.05),
  xlab = "x",
  ylab = "f(x)",
  main = "Reverse-order simulation"
)

points(
  x_known,
  y_known,
  pch = 19
)
