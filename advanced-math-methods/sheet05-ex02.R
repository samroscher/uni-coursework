# advanced mathematical methods
# sheet 05 exercise 2
# Topics: QR decomposition, Householder reflections, orthogonal matrices,
#         iterative elimination of sub-diagonal entries

# We construct the QR decomposition A = Q R via Householder reflections.
# Each step k multiplies from the left with an orthogonal matrix Q_k that
# zeros out the sub-diagonal entries in column k, while leaving the first
# (k-1) columns unchanged. After n steps: R = Q_n ... Q_1 A (upper tri),
# Q = Q_1^T ... Q_n^T (orthogonal).

# ---- (a) ----
# Write C in block form:
#   C = [C11, C12; 0, C22]
# where the zero block exists because the first (k-1) columns are already
# in upper triangular form. Then
#   Q_k C = [I, 0; 0, H] [C11, C12; 0, C22] = [C11, C12; 0, H C22].
# The first (k-1) columns are unchanged.

# ---- (b) ----
# The k-th column of C splits as (c_tilde, c_bar) with c_bar in R^{m-k+1}.
# By Aufgabe 5.1 (b), H_{c_bar - ||c_bar|| e1} c_bar in span(e1),
# so entries k+1, ..., m of Q_k c_k are zero.

# ---- (c) ----
# Naive implementation: explicitly builds Q_k as a full m x m matrix.

my_qr <- function(A) {
  m <- nrow(A)
  n <- ncol(A)
  Q <- diag(m)
  A_k <- A
  for (k in seq_len(min(m, n))) {
    c_bar <- A_k[k:m, k]
    v <- c_bar + c(norm(c_bar, "2"), rep(0, length(c_bar) - 1))
    H <- diag(m - k + 1) - 2 * tcrossprod(v, v) / sum(v ^ 2)
    # build full Q_k
    zero <- matrix(0, k - 1, m - k + 1)
    Q_k <- rbind(
      cbind(diag(k - 1), zero),
      cbind(t(zero), H)
    )
    A_k <- Q_k %*% A_k
    Q <- Q %*% t(Q_k)
  }
  list(Q = Q, R = A_k)
}

# Test on a random matrix
set.seed(5)
m <- 3
n <- 2
A <- matrix(rnorm(m * n), m, n)
A

qr_res <- my_qr(A)
qr_res

all.equal(qr_res$Q %*% qr_res$R, A)           # TRUE
all.equal(diag(m), qr_res$Q %*% t(qr_res$Q))   # Q orthogonal

# ---- Bonus: efficient implementation ----
# The naive version applies Q_k to the entire matrix, but only rows k:m and
# columns k:n are affected. We also choose the sign of the pivot to avoid
# catastrophic cancellation in v[1], and set the k-th column of R exactly
# to avoid accumulating rounding errors.

my_qr2 <- function(A) {
  m <- nrow(A)
  n <- ncol(A)
  Q <- diag(m)
  A_k <- A
  for (k in seq_len(min(m, n))) {
    c_bar <- A_k[k:m, k]
    c_bar_norm <- norm(c_bar, "2")
    # sign choice: v = c_bar + sign(c_bar[1]) * ||c_bar|| * e1
    v <- c_bar + c(sign(c_bar[1]) * c_bar_norm, rep(0, length(c_bar) - 1))
    H <- diag(m - k + 1) - 2 * tcrossprod(v, v) / sum(v ^ 2)
    # set k-th column exactly (no rounding error accumulation)
    A_k[k:m, k] <- c(-sign(c_bar[1]) * c_bar_norm, rep(0, length(c_bar) - 1))
    if (k < n)
      A_k[k:m, (k + 1):n] <- H %*% A_k[k:m, (k + 1):n]
    # update only affected columns of Q; H is symmetric so H = t(H)
    Q[, k:m] <- Q[, k:m] %*% H
  }
  list(Q = Q, R = A_k)
}

qr_res2 <- my_qr2(A)
qr_res2

all.equal(qr_res2$Q %*% qr_res2$R, A)            # TRUE
all.equal(diag(m), qr_res2$Q %*% t(qr_res2$Q))    # Q orthogonal

# ---- Benchmark: naive vs efficient ----
library(microbenchmark)

A_big <- matrix(rnorm(100 * 100), 100, 100)
microbenchmark(my_qr(A_big), my_qr2(A_big), times = 10)

# my_qr2 is roughly 3x faster because it skips redundant multiplications
# on the first (k-1) rows/columns at each step.