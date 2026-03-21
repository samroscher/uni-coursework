# advanced mathematical methods
# sheet 04 exercise 4
# Topics: SVD, denoising via truncated SVD, overfitting, regularization,
#         elbow heuristic in singular value spectrum

# We construct a rank-r matrix A = sum sigma_i u_i v_i^T, add Gaussian noise,
# and show that the truncated SVD at rank r recovers A better than any other k.

# ---- Setup: construct rank-r matrix ----
library(pracma)

n <- 20
r <- 3
sing_values <- seq(from = 5, to = 2, length.out = r)

set.seed(12345)
V <- gramSchmidt(matrix(rnorm(n * r), ncol = r))$Q
U <- gramSchmidt(matrix(rnorm(n * r), ncol = r))$Q

A <- matrix(0, nrow = n, ncol = n)
for (i in 1:r) {
  A <- A + sing_values[i] * outer(U[, i], V[, i])
}

# Sanity check: svd(A) should recover our singular values and vectors.
svd_A <- svd(A)
svd_A$d[1:5]  # first r match sing_values, rest ≈ 0

# ---- Add noise ----
std_dev <- 0.2
A_noisy <- A + matrix(rnorm(n^2, sd = std_dev), nrow = n)

# ---- SVD of noisy matrix ----
svd_noisy <- svd(A_noisy)

# Singular value spectrum: expect an "elbow" at index r = 3.
plot(1:n, svd_noisy$d, xlab = "index", ylab = "singular value")
plot(1:n, log(svd_noisy$d), xlab = "index", ylab = "log(singular value)")

# ---- Approximation errors for k = 1, ..., n ----
# NOTE on diag(): for k = 1, diag(scalar) creates a 1x1 matrix only if we
# explicitly set ncol. Otherwise diag(5) would create a 5x5 identity matrix.
# Using diag(x = ..., ncol = k) is safe for all k >= 1.

res <- data.frame(k = 1:n, error_noisy = NA, error_A = NA)

for (k in 1:n) {
  approx_k <- as.matrix(svd_noisy$u[, 1:k]) %*%
    diag(x = svd_noisy$d[1:k], ncol = k) %*%
    t(as.matrix(svd_noisy$v[, 1:k]))
  res$error_noisy[k] <- norm(A_noisy - approx_k, type = "2")
  res$error_A[k]     <- norm(A - approx_k, type = "2")
}

# ---- Plot ----
library(ggplot2)
library(tidyr)

res_long <- pivot_longer(res, cols = c("error_noisy", "error_A"))

ggplot(res_long, aes(x = k, y = value, col = name)) +
  geom_point() + geom_line() + theme_bw()

# ---- Observations ----
# - error_noisy decreases monotonically (by the Eckart-Young theorem,
#   ||A_noisy - A_noisy,k||_2 = sigma_{k+1}).
# - error_A has a MINIMUM at k = r = 3. For k < r we under-approximate the
#   true signal; for k > r we start fitting the noise (overfitting).
# - The elbow in the singular value spectrum at index r separates signal
#   from noise. Truncating at the elbow = regularization: slightly worse
#   fit to the noisy data, but much better recovery of the true matrix.