# advanced mathematical methods
# sheet 06 exercise 1
# Topics: QR algorithm, eigenvalue computation, similarity transformation,
#         iterative convergence to upper triangular form

# The QR algorithm iteratively transforms A into a similar upper triangular
# matrix whose diagonal entries are the eigenvalues of A.
# Starting from A_0 = A, each step computes A_k = Q_k R_k (QR decomposition)
# and sets A_{k+1} = R_k Q_k. After K steps, diag(A_K) ≈ eigenvalues of A.

# ---- (a) ----
# A_{k+1} = R_k Q_k = Q_k^T (Q_k R_k) Q_k = Q_k^T A_k Q_k.
# So A_{k+1} is similar to A_k. By transitivity of similarity (induction),
# A_{k+1} is similar to A_0 = A for all k. Similar matrices share eigenvalues.

# ---- (b) ----
qr_algorithm <- function(A, steps = 20) {
  A_k <- A
  for (k in 1:steps) {
    QR_k <- qr(A_k)
    Q_k <- qr.Q(QR_k)
    R_k <- qr.R(QR_k)
    A_k <- R_k %*% Q_k
  }
  diag(A_k)
}

# ---- (c) ----
# A diagonalizable matrix with eigenvalues lambda_i = i has the form
# A = V Lambda V^{-1} for arbitrary invertible V.

library(wordspace)

set.seed(0)
n <- 8
M <- matrix(rnorm(n ^ 2), nrow = n)
V <- normalize.cols(M)   # normalizing columns for numerical stability
L <- diag(seq(1, n))
A <- V %*% L %*% solve(V)

# ---- (d) ----
options(digits = 16)

qr_algorithm(A, steps = 10)
qr_algorithm(A, steps = 25)
qr_algorithm(A, steps = 50)
qr_algorithm(A, steps = 75)
qr_algorithm(A, steps = 100)

# The approximated eigenvalues converge to 1, 2, ..., 8 with increasing
# iterations. Internally, A_k approaches an upper triangular matrix whose
# diagonal entries are the eigenvalues. Convergence is geometric, with the
# rate depending on the ratios |lambda_i / lambda_{i+1}|.