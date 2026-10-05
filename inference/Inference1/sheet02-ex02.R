# statistical inference 1
# sheet 02 exercise 2.2
# Topics: divergence measures between densities, log-ratio,
#         absolute difference, comparison with KL divergence

# Given two normal densities f and g, we first plot them for different
# means and standard deviations (a). We then plot the log-ratio and the
# absolute difference of f and g (b), turn both into divergence measures
# by integrating over y (c), and compare them with the KL divergence (d).

# plotmath label such as "f ~ N(0, 1)", parametrised by the variance
dens_label <- function(name, mu, sd) {
  bquote(.(as.name(name)) %~% N(.(mu), .(sd^2)))
}

# ---- (a) ----
plot_densities <- function(mu_f, sd_f, mu_g, sd_g) {
  x <- seq(-6, 6, length.out = 500)
  f <- dnorm(x, mean = mu_f, sd = sd_f)
  g <- dnorm(x, mean = mu_g, sd = sd_g)

  plot(x, f, type = "l", col = "blue", lwd = 2,
       ylim = c(0, max(f, g)), ylab = "density",
       main = "Normal densities")
  lines(x, g, col = "red", lwd = 2)
  legend("topleft", bty = "n", col = c("blue", "red"), lwd = 2,
         legend = as.expression(list(dens_label("f", mu_f, sd_f),
                                     dens_label("g", mu_g, sd_g))))
}

plot_densities(mu_f = 0, sd_f = 1, mu_g = 0.5, sd_g = 1)
plot_densities(mu_f = 0, sd_f = 1, mu_g = 3, sd_g = 2)

# ---- (b) ----
plot_integrands <- function(mu_f, sd_f, mu_g, sd_g) {
  x <- seq(-6, 6, length.out = 500)
  f <- dnorm(x, mean = mu_f, sd = sd_f)
  g <- dnorm(x, mean = mu_g, sd = sd_g)

  log_ratio <- log(f / g)
  abs_diff <- abs(f - g)

  old_par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  on.exit(par(old_par))

  plot(x, log_ratio, type = "l", lwd = 2, ylab = "log(f / g)",
       main = "Log-ratio")
  abline(h = 0, lty = 2)
  plot(x, abs_diff, type = "l", lwd = 2, ylab = "|f - g|",
       main = "Absolute difference")
  mtext(bquote(.(dens_label("f", mu_f, sd_f)) ~ "vs." ~
                 .(dens_label("g", mu_g, sd_g))),
        outer = TRUE, line = 0.5)
}

plot_integrands(mu_f = 0, sd_f = 1, mu_g = 0.5, sd_g = 1)
plot_integrands(mu_f = 0, sd_f = 1, mu_g = 3, sd_g = 2)

# Equal sds: the log-ratio is a straight line in x; unequal sds: a parabola.
# Either way it does not decay in the tails. The absolute difference
# vanishes in the tails, since both densities go to zero there.

# ---- (c) ----
# log-ratio divergence: integral of log(f/g) over [-L, L]; we let L grow
# to check whether the integral converges. Log-densities are used since
# for large L both densities underflow to 0 and log(f / g) gives NaN.
log_ratio_div <- function(mu_f, sd_f, mu_g, sd_g, L) {
  integrand <- function(x) {
    dnorm(x, mean = mu_f, sd = sd_f, log = TRUE) -
      dnorm(x, mean = mu_g, sd = sd_g, log = TRUE)
  }
  integrate(integrand, lower = -L, upper = L)$value
}

L_values <- c(5, 10, 50, 100)
sapply(L_values, log_ratio_div, mu_f = 0, sd_f = 1, mu_g = 0.5, sd_g = 1)
# 1.25  2.50  12.50  25.00
sapply(L_values, log_ratio_div, mu_f = 0, sd_f = 1, mu_g = 3, sd_g = 2)
# -13.07  -213.64  -31068.19  -249636.37

# absolute-difference divergence: 0.5 * integral of |f - g|. Integrating
# over +-10 sds around both means covers all the mass; integrate() over
# (-Inf, Inf) can miss narrow peaks far from 0 and return 0.
abs_diff_div <- function(mu_f, sd_f, mu_g, sd_g) {
  integrand <- function(x) {
    abs(dnorm(x, mean = mu_f, sd = sd_f) - dnorm(x, mean = mu_g, sd = sd_g))
  }
  lower <- min(mu_f - 10 * sd_f, mu_g - 10 * sd_g)
  upper <- max(mu_f + 10 * sd_f, mu_g + 10 * sd_g)
  0.5 * integrate(integrand, lower = lower, upper = upper)$value
}

abs_diff_div(mu_f = 0, sd_f = 1, mu_g = 0.5, sd_g = 1)  # 0.197
abs_diff_div(mu_f = 0, sd_f = 1, mu_g = 3, sd_g = 2)    # 0.708

# The log-ratio integral grows without bound as L increases: linearly for
# equal sds (0.25 * L here), cubically for unequal sds (negative, since the
# wider g dominates in the tails). So it does not converge.
# The absolute-difference divergence is finite and lies in [0, 1];
# 0.197 means f and g share about 80% of their probability mass.

# ---- (d) ----
# KL divergence as the log-ratio weighted by f. Since the weight f is
# negligible beyond +-10 sds of mu_f, integrating there suffices.
kl_div <- function(mu_f, sd_f, mu_g, sd_g) {
  integrand <- function(x) {
    dnorm(x, mean = mu_f, sd = sd_f) *
      (dnorm(x, mean = mu_f, sd = sd_f, log = TRUE) -
         dnorm(x, mean = mu_g, sd = sd_g, log = TRUE))
  }
  integrate(integrand, lower = mu_f - 10 * sd_f,
            upper = mu_f + 10 * sd_f)$value
}

# f = N(0, 1), g = N(delta, 1): move g further and further away
deltas <- c(0.5, 2, 10, 100, 2000)
comparison <- data.frame(
  delta    = format(deltas, scientific = FALSE, drop0trailing = TRUE),
  abs_diff = round(sapply(deltas, function(d) abs_diff_div(0, 1, d, 1)), 3),
  kl       = format(sapply(deltas, function(d) kl_div(0, 1, d, 1)),
                    scientific = FALSE, drop0trailing = TRUE, big.mark = ",")
)
print(comparison, row.names = FALSE)

# KL is not symmetric: swapping f and g changes the value
kl_div(mu_f = 0, sd_f = 1, mu_g = 3, sd_g = 2)  # KL(N(0, 1), N(3, 4)) = 1.443
kl_div(mu_f = 3, sd_f = 2, mu_g = 0, sd_g = 1)  # KL(N(3, 4), N(0, 1)) = 5.307

# The log-ratio divergence is useless, since its integral does not exist.
# The absolute-difference divergence is finite but saturates at 1: once
# the densities no longer overlap (delta around 10), it cannot tell
# delta = 10 from delta = 2000. KL keeps the log-ratio's sensitivity but
# stays finite, because the weight f kills the growth in the tails. For
# equal sds it equals delta^2 / (2 sigma^2), so it grows without bound.