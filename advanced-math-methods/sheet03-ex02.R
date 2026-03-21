# advanced mathematical methods
# sheet 03 exercise 2
# Topics: LU decomposition, numerical stability, column pivoting,
#         condition numbers, floating-point arithmetic

# Given A = [eps, 1; 1, 1] with 0 < eps << 1.
# We investigate how a tiny pivot element causes catastrophic cancellation
# in the LU decomposition without pivoting, and how column pivoting fixes it.

# ---- (a) ----
# A single elimination step gives A = L * U with
#   L = [1, 0; 1/eps, 1],  U = [eps, 1; 0, 1 - 1/eps].

# ---- (b) ----
# The problem lies in u22 = 1 - 1/eps. For eps = 1e-17 on a machine with
# ~16 significant decimal digits, 1 - 1e17 rounds to -1e17, so the
# computed U becomes [eps, 1; 0, -1/eps]. Multiplying L * U_hat gives
# [eps, 1; 1, 0] instead of [eps, 1; 1, 1] -- the (2,2) entry is lost.

library(pracma)

A <- function(eps) matrix(c(eps, 1, 1, 1), nrow = 2)

# eps = 0.1: no problem
A1 <- A(eps = 1e-1)
lu1 <- lu(A1)
lu1$L %*% lu1$U   # recovers A1 correctly

# eps = 1e-17: catastrophic cancellation
A2 <- A(eps = 1e-17)
lu2 <- lu(A2)
lu2$L %*% lu2$U   # (2,2) entry becomes 0 instead of 1

# ---- (c) ----
# Condition numbers w.r.t. the maximum norm.
# For a matrix M: kappa_inf(M) = ||M||_inf * ||M^{-1}||_inf
# where ||M||_inf = max row sum of |M|.

eps <- 1e-17

# A itself is well-conditioned:
#   ||A||_inf = 2,  ||A^{-1}||_inf = 2 / (1 - eps) ≈ 2
#   => kappa_inf(A) ≈ 4.
norm(A(eps), type = "I") * norm(solve(A(eps)), type = "I")

# L is ill-conditioned:
#   ||L||_inf = 1/eps + 1,  ||L^{-1}||_inf = 1/eps + 1
#   => kappa_inf(L) = (1/eps + 1)^2.
L <- lu2$L
norm(L, type = "I") * norm(solve(L), type = "I")

# U is ill-conditioned:
#   ||U||_inf ≈ 1/eps,  ||U^{-1}||_inf ≈ 1/eps
#   => kappa_inf(U) ≈ 1/eps^2.
U <- lu2$U
norm(U, type = "I") * norm(solve(U), type = "I")

# A is well-conditioned, but L and U are catastrophically ill-conditioned.
# The factorization amplifies rounding errors.

# ---- (d) ----
# Column pivoting swaps rows so the largest-magnitude element becomes the
# pivot. Permutation matrix P = [0, 1; 1, 0] gives
#   P * A = [1, 1; eps, 1] = L_tilde * U_tilde
# with L_tilde = [1, 0; eps, 1],  U_tilde = [1, 1; 0, 1 - eps].
# Now 1 - eps ≈ 1 -- no cancellation.

# ---- (e) ----
# After pivoting, all three matrices are well-conditioned:
#   kappa_inf(P * A) ≈ 4,  kappa_inf(L_tilde) ≈ 1,  kappa_inf(U_tilde) ≈ 4.

P <- matrix(c(0, 1, 1, 0), nrow = 2)
A_tilde <- P %*% A(eps)
lu_tilde <- lu(A_tilde)

norm(A_tilde, type = "I") * norm(solve(A_tilde), type = "I")
norm(lu_tilde$L, type = "I") * norm(solve(lu_tilde$L), type = "I")
norm(lu_tilde$U, type = "I") * norm(solve(lu_tilde$U), type = "I")

# All condition numbers are O(1). Column pivoting was successful.