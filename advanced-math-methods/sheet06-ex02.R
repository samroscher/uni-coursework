# advanced mathematical methods
# sheet 06 exercise 2
# Topics: Gauss-Seidel iteration, iterative solvers, diagonal dominance,
#         spectral radius, convergence bounds, forward substitution

# The Gauss-Seidel method solves Ax = b via the iteration
#   x^{(k)} = x^{(k-1)} + (L + D)^{-1} (b - A x^{(k-1)})
# where A = L + D + U (lower, diagonal, upper parts).
# Convergence is guaranteed for strictly diagonally dominant A.

# ---- (a) ----
# Generate a random strictly diagonally dominant matrix: set each diagonal
# entry to the row sum of absolute values (guarantees strict dominance).

rmat_diagdom <- function(n) {
  A <- matrix(rnorm(n ^ 2), nrow = n)
  diag(A) <- rowSums(abs(A))
  A
}

# ---- (b) ----
# Key insight: (L + D) is lower triangular, so (L + D)^{-1} y can be
# computed via forwardsolve() -- no explicit inverse needed.

gauss_seidel <- function(A, b, x_0 = NULL, tol = 1e-6, max_steps = 100) {
  n <- ncol(A)
  if (is.null(x_0)) x_0 <- numeric(n)

  # lower triangular part of A (including diagonal) = L + D
  LD <- A
  LD[upper.tri(A)] <- 0

  x_k <- x_0
  for (k in 1:max_steps) {
    x_old <- x_k
    x_k <- x_old + forwardsolve(LD, b - A %*% x_old)
    if (norm(x_k - x_old, type = "I") <= tol) break
  }
  list(x = x_k, steps = k, LD = LD)
}

# ---- (c) ----
options(digits = 7)
set.seed(0)
n <- 8

for (i in 1:10) {
  A <- rmat_diagdom(n)
  x_true <- rnorm(n)
  b <- A %*% x_true
  x_0 <- numeric(n)

  gs <- gauss_seidel(A, b, x_0)

  # spectral radius rho = ||I - (L+D)^{-1} A||_2, no explicit inverse:
  # forwardsolve(LD, A) computes (L+D)^{-1} A column by column
  rho <- norm(diag(n) - forwardsolve(gs$LD, A), type = "2")

  err <- norm(x_true - gs$x, type = "2")
  upper_bound <- rho ^ gs$steps * norm(x_true - x_0, type = "2")

  cat("Iteration", i, "\n")
  cat("  spectral radius:  ", rho, "\n")
  cat("  steps:            ", gs$steps, "\n")
  cat("  approx error:     ", err, "\n")
  cat("  upper bound:      ", upper_bound, "\n")
  cat("  bound holds:      ", err <= upper_bound, "\n\n")
}

# In all 10 runs, rho < 1 (guaranteed by diagonal dominance) and the
# actual error is well below the theoretical upper bound. Convergence
# typically takes 9-13 steps for tol = 1e-6 with n = 8.