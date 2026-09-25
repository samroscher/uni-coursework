# statistical inference 2
# sheet 3 exercise 3.1
# Topics: nonparametric bootstrap, IQR, confidence intervals

# Bootstrap the sample IQR for data from a two-component normal mixture,
# estimate its standard error and skewness, and compare two bootstrap CIs.


set.seed(121)


# Common settings
n <- 120
B <- 500
alpha <- 0.05



# ---- (a) ----
# Draw one sample from 0.6 N(0,1) + 0.4 N(5,2^2)
# and compute the observed IQR.

x <- ifelse(
  runif(n) < 0.6,
  rnorm(n, 0, 1),
  rnorm(n, 5, 2)
)

I_obs <- quantile(x, 0.75) - quantile(x, 0.25)

I_obs



# ---- (b) ----
# Draw B bootstrap samples and compute the IQR for each.

I_star <- replicate(B, {

  x_star <- sample(
    x,
    size = n,
    replace = TRUE
  )

  quantile(x_star, 0.75) - quantile(x_star, 0.25)
})


se_boot <- sd(I_star)
skewness <- e1071::skewness(I_star)

se_boot
skewness



# ---- (c) ----
# Plot the bootstrap distribution.

hist(
  I_star,
  probability = TRUE,
  breaks = 30,
  main = "Bootstrap distribution of the IQR",
  xlab = "IQR*",
  ylab = "Density",
  border = "grey"
)

lines(
  density(I_star),
  lwd = 2
)

abline(
  v = I_obs,
  lwd = 2,
  lty = 2
)

rug(I_star)



# ---- (d) ----
# Compare a percentile bootstrap CI with a normal-approximation CI.

ci_percentile <- quantile(
  I_star,
  probs = c(alpha / 2, 1 - alpha / 2)
)

ci_normal <- I_obs +
  c(-1, 1) * qnorm(1 - alpha / 2) * se_boot


ci_percentile
ci_normal

diff(ci_percentile)
diff(ci_normal)


abline(
  v = ci_percentile,
  lwd = 2,
  lty = 3
)

abline(
  v = ci_normal,
  lwd = 2,
  lty = 4
)

legend(
  "topleft",
  legend = c(
    "Observed IQR",
    "Percentile CI",
    "Normal CI"
  ),
  lty = c(2, 3, 4),
  lwd = 2,
  bty = "n"
)
