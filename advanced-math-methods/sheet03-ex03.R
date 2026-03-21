# advanced mathematical methods
# sheet 03 exercise 3
# Topics: Cholesky decomposition, solving SPD systems, forward/back
#         substitution, benchmarking, computational complexity

# Given M in R^{500x500} with iid N(0,1) entries, A = M M^T is (a.s.) SPD.
# We exploit the Cholesky factorization A = R^T R to solve A^{-1} B
# more efficiently than the general-purpose solve().

set.seed(5)
M <- matrix(rnorm(500 * 500), 500, 500)
A <- M %*% t(M)

c(
  all.equal(A, t(A)),        # A is symmetric
  min(eigen(A)$values) > 0   # A is positive definite
)

# ---- (a) ----
# A^{-1} B = X  <=>  A X = B  <=>  R^T R X = B.
# Step 1: Cholesky factorization A = R^T R  (chol() returns upper-triangular R).
# Step 2: Solve R^T Y = B  by forward substitution (backsolve with transpose).
# Step 3: Solve R X = Y    by back substitution.
#
# NOTE: R's chol(A) returns the UPPER triangular factor R with A = t(R) %*% R.
# The PDF solution incorrectly passes this to forwardsolve(), which expects a
# lower-triangular matrix. We use backsolve(..., transpose = TRUE) instead,
# which solves t(R) %*% Y = B without forming t(R) explicitly.

spd_solve <- function(A, B) {
  R <- chol(A)                            # A = t(R) %*% R, R upper-triangular
  Y <- backsolve(R, B, transpose = TRUE)  # solve t(R) %*% Y = B
  backsolve(R, Y)                         # solve R %*% X = Y
}

# Quick sanity check:
B_check <- rnorm(500)
max(abs(spd_solve(A, B_check) - solve(A, B_check)))

# ---- (b) ----
library(microbenchmark)

B <- rnorm(500)
microbenchmark(solve(A, B), spd_solve(A, B))

# spd_solve() is roughly twice as fast because the Cholesky factorization
# of an SPD matrix costs about n^3 / 3 flops vs. 2 n^3 / 3 for LU.

# ---- (c) ----
B_1   <- rnorm(500)
B_10  <- matrix(rnorm(500 * 10),   500, 10)
B_100 <- matrix(rnorm(500 * 100),  500, 100)  # PDF has 500*10 here (typo)

microbenchmark(
  spd_solve(A, B_1),
  spd_solve(A, B_10),
  spd_solve(A, B_100)
)

# Runtime grows slowly with k because the dominant cost is the Cholesky
# factorization O(n^3), which is independent of k. The triangular solves
# add O(k n^2). Only when k ≈ n does the solve cost become noticeable.

B_500  <- matrix(rnorm(500 * 500),  500, 500)
B_1000 <- matrix(rnorm(500 * 1000), 500, 1000)

microbenchmark(
  spd_solve(A, B_100),
  spd_solve(A, B_500),
  spd_solve(A, B_1000)
)

# For large k the runtime increases approximately linearly in k,
# confirming the overall complexity O(n^3 + k n^2).