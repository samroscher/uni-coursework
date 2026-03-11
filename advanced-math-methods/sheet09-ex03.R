# advanced mathematical methods
# sheet 9 exercise 3
# Topics: Taylor approximation, higher-order derivatives, polynomial approximation

# Task:
# Compute the Taylor approximation of degree k at x0 = 0 for f(x) = exp(x) and
# f(x) = sin(x):
#
#   f(x) ≈ f(x0) + sum_{j=1}^{k} (1/j!) * f^{(j)}(x0) * (x - x0)^j
#
# Implement a function in R that returns the Taylor approximation of degree k,
# and plot the approximation for various k alongside the true function.
#
# Hint: Write a function that returns a function (closure). Use factorial() for j!.

# Description: Creates a Taylor approximation of exp(x) around x0 = 0 for a given degree k.
# Arguments:   k -- integer, degree of the Taylor polynomial
# Returns:     function, takes a numeric vector x and returns the Taylor approximation of exp(x)
taylor_exp <- function(k) {
  f <- function(x) {
    # define the Taylor approximation of degree k as a function of x here.
    y <- 1 # exp(0)
    for (j in seq_len(k)) {
      y <- y + (x ^ j) / factorial(j)
    }
    y
  }
  f
}

k <- 1
taylor <- taylor_exp(k) # taylor is now a function of x

# taylor approximation of sin(x)
taylor_sin <- function(k) {
  f <- function(x) {
    y <- 0
    if (k == 0) return(y)
    idx <- seq(from = 1, to = k, by = 2)
    pos_neg <- +1
    for (j in idx) {
      y <- y + (pos_neg * x ^ j) / factorial(j)
      pos_neg <- pos_neg * (-1)
    }
    y
  }
  f
}

# plots
library(ggplot2)
theme_set(theme_minimal())
library(cowplot)

# --- Plot for exp(x) ---
plot_list_exp <- list()
k_values_exp <- 1:9
x <- seq(-5, 3, by = 0.1)

for (i in seq_along(k_values_exp)) {
  k <- k_values_exp[[i]]
  taylor <- taylor_exp(k)
  dat <- data.frame(x = x, true = exp(x), approx = taylor(x))

  plot_list_exp[[i]] <- ggplot(dat) +
    geom_line(aes(x = x, y = true), col = "blue") +
    geom_line(aes(x = x, y = approx), col = "red", linetype = "dashed") +
    labs(title = paste0("k = ", k), y = "y") +
    coord_cartesian(ylim = c(-5, 20))
}

plot_grid(plotlist = plot_list_exp, ncol = 3)

# --- Plot for sin(x) ---
plot_list_sin <- list()
k_values_sin <- seq(1, 17, by = 2)
x <- seq(-10, 10, by = 0.5)

for (i in seq_along(k_values_sin)) {
  k <- k_values_sin[[i]]
  taylor <- taylor_sin(k)
  dat <- data.frame(x = x, true = sin(x), approx = taylor(x))

  plot_list_sin[[i]] <- ggplot(dat) +
    geom_line(aes(x = x, y = true), col = "blue") +
    geom_line(aes(x = x, y = approx), col = "red", linetype = "dashed") +
    labs(title = paste0("k = ", k), y = "y") +
    coord_cartesian(ylim = c(-2.5, 2.5))
}

plot_grid(plotlist = plot_list_sin, ncol = 3)

# not part of the exercise:
# gif animation
# install.packages("gganimate")
library(gganimate)

x_exp <- seq(-5, 3, by = 0.1)
dat_all <- do.call(rbind, lapply(k_values_exp, function(k) {
  taylor <- taylor_exp(k)
  data.frame(x = x_exp, true = exp(x_exp), approx = taylor(x_exp), k = k)
}))

anim <- ggplot(dat_all) +
  geom_line(aes(x = x, y = true), col = "blue") +
  geom_line(aes(x = x, y = approx), col = "red", linetype = "dashed") +
  labs(title = "Taylor Approximation of exp(x), k = {closest_state}", y = "y") +
  coord_cartesian(ylim = c(-5, 20)) +
  transition_states(k, transition_length = 4, state_length = 3)

animate(anim, fps = 10, nframes = length(k_values_exp) * 15)
