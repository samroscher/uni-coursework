# advanced mathematical methods
# sheet 14 exercise 1
# Topics: Smoothing with cubic splines, bias-variance trade-off,
#         MSE decomposition

# Recap:
# Smoothing instead of interpolation: we observe noisy data
#   y_i = f(x_i) + eps_i
# and want to estimate f by f_hat(x) = sum_{k=1}^{K} a_k phi_k(x) with
# given basis functions phi_k. As last week, we need to determine the
# coefficients a_k.
# We again obtain the linear system B a = y (B in R^{n x K}), but now
# with n > K (interpolation: n = K). We therefore solve the least-squares
# problem min_a ||B a - y||.
# As basis functions one usually chooses cubic splines. The number of
# basis functions K (i.e. the number of knots) plays a central role:
# if K is too small, the splines are too inflexible to approximate the
# true f (high bias). If K is too large, the splines adapt to the random
# noise of the eps_i (high variance) -> bias-variance trade-off.

# We want to smooth given data (x_i, y_i) with y_i = f(x_i) + eps_i
# using cubic splines with K basis functions. For the estimated function
# f_hat the mean squared error (MSE) is the sum of the squared bias and
# the variance:
#   E[(f_hat(x) - f(x))^2] = (E[f_hat(x)] - f(x))^2 + E[(f_hat(x) - E[f_hat(x)])^2]
#          MSE                      Bias^2                       Variance
# (Note: for each fixed x; the random quantity here is f_hat.)
# The squared bias decreases when K increases, while the variance
# increases when K increases. The optimal choice of K (i.e. the K that
# minimizes the MSE) is therefore unclear. We want to examine the
# relationship between bias, variance and MSE as a function of K for a
# concrete data example.

library(ggplot2)
library(tidyr)
# ---- (a) ----
# Write a function fit_spline that, for given data x, y, fits a cubic
# spline with K basis functions (argument df) and evaluates the estimated
# function on the values xnew. fit_spline should return the vector of
# estimated y_hat_new.
# Hint: the LS problem min_a ||B a - y|| can be solved with qr.solve(B, y).

set.seed(42)
f_true <- function(x) exp(- x ^ 2) + sin(x) - cos(2 * x)
n <- 100
sd <- 0.25
x <- seq(- 5, 5, length = n)
y <- f_true(x) + rnorm(n, sd = sd)

xnew <- seq(- 5, 5, by = 0.05)
plot(xnew, f_true(xnew), type = "l", lwd = 2)
points(x, y, col = "grey")

fit_spline <- function(x, y, K, xnew) {
  # Spline fitting is a two-step process:
  #
  # Step 1 — Build the basis matrix B (only needs x, not y).
  #   bs() evaluates K B-spline basis functions phi_1, ..., phi_K at the
  #   points x_1, ..., x_n. The result is B in R^{n x K} with
  #   B[i, k] = phi_k(x_i). This is analogous to building a Vandermonde
  #   matrix [1, x, x^2, ...] for polynomial regression: the "design
  #   matrix" depends only on where we evaluate, not on the response y.
  #   intercept = TRUE keeps all K columns; without it bs() drops one
  #   (like model.matrix does for lm), which would break direct solving.
  #### (a) ToDo ####
  B <- splines::bs(x, df = K, intercept = TRUE)

  # Step 2 — Determine coefficients a (needs B and y).
  #   Solves min_a ||B a - y|| via QR decomposition. The resulting a
  #   defines our estimated function: f_hat(x) = sum_k a_k phi_k(x).
  a <- qr.solve(B, y)

  # Step 3 — Evaluate f_hat at new points xnew.
  #   We need a new basis matrix B_new evaluated at xnew, but it must use
  #   the SAME knots and boundary knots as B.
  #
  #   Why? The knots determine WHICH basis functions phi_1, ..., phi_K
  #   exist. Different knots -> different phi_k -> different basis.
  #   Our coefficients a were solved for the specific equation
  #     f_hat(x) = a_1 phi_1(x) + ... + a_K phi_K(x).
  #   If B_new uses different knots, it contains different basis functions
  #   phi_tilde_k. Then B_new %*% a computes
  #     a_1 phi_tilde_1(x) + ... + a_K phi_tilde_K(x)
  #   which is nonsense: coefficients fitted for one basis, applied to
  #   another. (Analogy: fitting a_1 + a_2 x + a_3 x^2, then plugging
  #   the same a into 1, x, x^3 — same number of basis functions, but
  #   the wrong ones.)
  #
  #   If we called bs(xnew, df = K, intercept = TRUE) without specifying
  #   knots, bs() would place new knots based on the quantiles of xnew,
  #   silently producing a different basis. Hence we extract and reuse.
  Bnew <- splines::bs(xnew,
                      knots = attr(B, "knots"),
                      intercept = TRUE,
                      Boundary.knots = attr(B, "Boundary.knots"))
  yhat <- c(Bnew %*% a)
  yhat
}

plot(xnew, f_true(xnew), type = "l")
lines(xnew, fit_spline(x, y, K = 5, xnew), col = "red", lty = 2)
lines(xnew, fit_spline(x, y, K = 10, xnew), col = "orange", lty = 3)
lines(xnew, fit_spline(x, y, K = 15, xnew), col = "purple", lty = 4)
lines(xnew, fit_spline(x, y, K = 40, xnew), col = "blue", lty = 5)
points(x, y, cex = 0.5, col = "grey")
legend("topright",
       legend = c("f_true", "K = 5", "K = 10", "K = 15", "K = 40"),
       col = c("black", "red", "orange", "purple", "blue"),
       lty = 1:5,
       cex = 0.8,
       bty = "n")

# Observations:
#   K =  5 (red):    underfitting — too few basis functions to capture the
#                     shape of f, high bias.
#   K = 10 (orange): better, but still visibly deviates from f_true in
#                     regions with more curvature.
#   K = 15 (purple): good fit — flexible enough to approximate f_true
#                     without adapting to noise.
#   K = 40 (blue):   overfitting — spline follows random noise (visible
#                     as wiggly oscillations), high variance.

# ---- (b) ----
# Now we want to estimate bias, variance and MSE. Fill in the gaps in
# the code. For K = 10, 11, ..., 40, N = 50 data sets should each be
# generated in the same way as above, the spline fitted and y_hat_new
# computed. All y_hat_new values should be stored in result_K.
#
# Then, for each xnew value (i.e. each row of result_K), the bias,
# variance and MSE of the N estimated f_hat^(j), j = 1, ..., N should
# be computed:
#   Bias^2(x)   = (1/N sum_{j=1}^{N} f_hat^(j)(x) - f(x))^2
#   MSE(x)      = 1/N sum_{j=1}^{N} (f_hat^(j)(x) - f(x))^2
#   Variance(x) = 1/N sum_{j=1}^{N} (f_hat^(j)(x) - f_bar(x))^2
#     with f_bar(x) = 1/N sum_{j=1}^{N} f_hat^(j)(x).
#
# We obtain bias, variance and MSE for each xnew value and take the
# mean of each, storing the values together with K in result.
# Afterwards the results are plotted.

K_vec <- seq(10, 40)
N <- 50

set.seed(42)
result <- data.frame()
ynew <- f_true(xnew)

for (K in K_vec) {
  result_K <- matrix(nrow = length(xnew), ncol = N)

  for (i in seq_len(N)) {
    y <- f_true(x) + rnorm(n, sd = sd)
    yhat <- fit_spline(x, y, K, xnew)
    result_K[, i] <- yhat
    ### generate random y as above
    ### compute yhat with fit_spline
    ### store yhat in result_K
  }
  # result_K should now have N columns

  error_matrix <- result_K - ynew  # subtracts (true) ynew from each column
  # helpful for bias and MSE
  # further hint: rowMeans function

  # Bias^2
  bias_x_squared <- rowMeans(error_matrix) ^ 2 ### ToDo
    # don't forget to square!

    # MSE
    MSE_x <- rowMeans(error_matrix ^ 2) ### ToDo

    # Variance
    variance_x <- rowMeans((result_K - rowMeans(result_K)) ^ 2) ### ToDo

  bias_squared_K <- mean(bias_x_squared)
  MSE_K <- mean(MSE_x)
  var_K <- mean(variance_x)

  result <- rbind(
    result,
    data.frame(K = K, bias_squared = bias_squared_K,
               MSE = MSE_K, var = var_K)
  )
}

# if you did everything correctly, this gives the zero vector
result$MSE - (result$bias_squared + result$var)

ggplot(
  data = pivot_longer(result, cols = 2:4),
  aes(x = K, y = value, col = name)) +
  geom_line() +
  geom_point() +
  theme_bw()
# plots bias, variance and MSE as a function of K
# Observations:
#   The sanity check result$MSE - (result$bias_squared + result$var) gives
#   (numerical) zeros, confirming MSE = Bias^2 + Variance.
#   The plot shows the classic bias-variance trade-off:
#     - Bias^2 decreases with K (more basis functions -> more flexible).
#     - Variance increases with K (more flexibility -> more sensitive to noise).
#     - MSE has a minimum around K = 12: the best compromise between
#       underfitting and overfitting.
