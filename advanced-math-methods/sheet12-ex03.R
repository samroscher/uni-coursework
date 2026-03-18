# advanced mathematical methods
# sheet 12 exercise 3
# Topics: Gradient Descent, Momentum, quadratic optimization, step size sensitivity

# Given: quadratic function f(x) = 0.5 * x^T A x + b^T x + c
# where A is symmetric positive definite, A in R^(n x n), b in R^n, c in R.
# We want to minimize f over x using gradient descent.

# Task:
# (a) Implement Gradient Descent for a given quadratic f (i.e. given A, b, c),
#     step size gamma, starting point x0, and max iterations N_max.
#     Output: matrix containing all x_k, k = 0, ..., N_max.
#     Hint: First determine the gradient of f using known matrix calculus rules.

# Gradient of f(x) = 0.5 * x^T A x + b^T x + c:
# grad f(x) = x^T A + b^T  (row vector, 1 x n)
# Since A is symmetric: A^T = A, so x^T A^T = x^T A.
# In code we use the transposed (column) form: Ax + b (n x 1).

# Description: Gradient Descent for a quadratic function
#              f(x) = 0.5 * x^T A x + b^T x + c with symmetric positive
#              definite A.
# Arguments:   A     -- numeric matrix (n x n), symmetric positive definite
#              b     -- numeric vector (n)
#              gamma -- numeric scalar, step size
#              x0    -- numeric vector (n), starting point
#              Nmax  -- integer, maximum number of iterations (default 200)
#              tol   -- numeric scalar, convergence tolerance (default 1e-10)
# Returns:     numeric matrix (k+1 x n), rows are iterates x_0, ..., x_k

gd_quadratic <- function(A, b, gamma, x0, Nmax = 200, tol = 1e-10) {

  results <- matrix(
    NA_real_,
    nrow = Nmax + 1,
    ncol = length(x0),
    dimnames = list(NULL, paste0("x", seq_along(x0)))
  )
  results[1, ] <- x0

  for (i in seq_len(Nmax)) {
    xk <- results[i, ]
    xk_next <- xk - gamma * (c(A %*% xk) + b)
    results[i + 1, ] <- xk_next
    if (sqrt(sum((xk - xk_next) ^ 2)) < tol) {
      cat(sprintf("Gradient Descent converged after %d iterations.\n", i))
      results <- results[1:(i + 1), ]
      break
    }
  }

  rownames(results) <- paste0("x_", 0:(nrow(results) - 1))
  return(results)
}

# (b) Apply Gradient Descent with:
#     A = matrix(c(1/5, 0, 0, 24), nrow = 2), b = c(0, 0), c = 0,
#     gamma = 0.08, x0 = c(10, 1), N_max = 20.
#     What convergence behavior (typical for GD) can be observed?
#     What happens with gamma = 0.1?

A <- matrix(c(1/5, 0, 0, 24), nrow = 2)
b <- c(0, 0)
gamma <- 0.08
x0 <- c(10, 1)
Nmax <- 20
tol <- 0

gd_quadratic(A, b, gamma, x0, Nmax, tol)

# for gamma = 0.08
# x_20  7.242725  0.1886933
# for gamma = 0.10
# x_20  6.676080  836.682554 (Divergence!)

# --- Contour plot for gamma = 0.08 ---
f <- function(x1, x2) 0.5 * (A[1,1] * x1 ^ 2 + A[2,2] * x2 ^ 2)

x1_grid <- seq(0, 10, 0.1)
x2_grid <- seq(-1.5, 1.5, 0.01)
z <- outer(x1_grid, x2_grid, f)

gd_08 <- gd_quadratic(A, b, gamma, x0, Nmax, tol)

contour(x1_grid, x2_grid, z, levels = c(0.1, 0.5, 1, 2, 5, 10, 20),
        xlab = "x1", ylab = "x2", main = "Gradient Descent (gamma = 0.08)")
lines(gd_08[, 1], gd_08[, 2])
points(gd_08[, 1], gd_08[, 2])

# --- Gamma = 0.1: divergence in x2 ---
# Contour plot omitted since x2 explodes to ~800, making visualization impractical.
# The key observation: gamma = 0.1 exceeds the stability threshold 2/lambda_max = 2/24 ≈ 0.083,
# causing divergence in the x2 direction.

# (c) Implement Gradient Descent with Momentum and apply with:
#     same A, b, c, x0, N_max as above, gamma = 0.06, alpha = 0.9.
#     Momentum update:
#       v^(k+1) = alpha * v^(k) - gamma * grad_f(x^(k))
#       x^(k+1) = x^(k) + v^(k+1)

gd_quadr_momentum <- function(A, b, gamma, alpha, x0, Nmax = 200, tol = 1e-10) {

  results <- matrix(
    NA_real_,
    nrow = Nmax + 1,
    ncol = length(x0),
    dimnames = list(NULL, paste0("x", seq_along(x0)))
  )
  results[1, ] <- x0
  vk <- rep(0, length(x0))

  for (i in seq_len(Nmax)) {
    xk <- results[i, ]
    vk_next <- alpha * vk - gamma * (c(A %*% xk) + b)
    xk_next <- xk + vk_next
    results[i + 1, ] <- xk_next
    vk <- vk_next

    if (sqrt(sum((xk - xk_next) ^ 2)) < tol) {
      cat(sprintf("Gradient Descent with momentum converged after %d iterations.\n", i))
      results <- results[1:(i + 1), ]
      break
    }
  }

  rownames(results) <- paste0("x_", 0:(nrow(results) - 1))
  return(results)
}

gd_quadr_momentum(A, b, gamma = 0.06, alpha = 0.9, x0, Nmax = 20, tol = 0)

# x_20  0.07649541 -0.18526128

# --- Contour plot: GD vs Momentum comparison ---
gd_path <- gd_quadratic(A, b, gamma = 0.08, x0, Nmax = 100, tol = 0)
mom_path <- gd_quadr_momentum(A, b, gamma = 0.06, alpha = 0.9, x0, Nmax = 100, tol = 0)

contour(x1_grid, x2_grid, z, levels = c(0.1, 0.5, 1, 2, 5, 10, 20),
        xlab = "x1", ylab = "x2", main = "GD vs Momentum (100 iterations)")
lines(gd_path[, 1], gd_path[, 2], col = "purple")
points(gd_path[, 1], gd_path[, 2], col = "purple", pch = 16, cex = 0.5)
lines(mom_path[, 1], mom_path[, 2], col = "coral", lty = 2)
points(mom_path[, 1], mom_path[, 2], col = "coral", pch = 16, cex = 0.5)
legend("topright", legend = c("GD (gamma=0.08)", "Momentum (gamma=0.06)"),
       col = c("purple", "coral"), lty = c(1, 2), cex = 0.8)
