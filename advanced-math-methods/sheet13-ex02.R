# advanced mathematical methods
# sheet 13 exercise 2
# Topics: Polynomial interpolation, monomial basis, orthogonal polynomial basis,
#         conditioning, Runge's phenomenon

# Given: n equidistant points (x, y) on [0, 1], y ~ Uniform(-1, 1).
#        We interpolate using a degree (n-1) polynomial in two bases:
#        (1) Monomial basis: phi_k(x) = x^k, k = 0, ..., n-1
#        (2) Orthogonal polynomial basis via poly()

# Task:
# (a) Compute coefficients a_M (monomial) and a_poly (orthogonal) by solving
#     the interpolation system B * a = y.
# (b) Evaluate and plot the resulting polynomials at new x values.
# (c) Compare the two interpolations — should they differ?
# (d) Increase n (e.g. n = 20). Observe: monomial basis becomes singular,
#     orthogonal basis remains stable, but Runge's phenomenon appears.

set.seed(1)
n <- 5
x <- seq(0, 1, length = n)
y <- runif(n, - 1, 1)

B_M <- outer(x, 0:(n - 1), "^")           # Monomial basis
B_poly_0 <- poly(x, degree = (n - 1))
B_poly <- cbind(rep(1, n), B_poly_0)      # Orthogonal polynomial basis

a_M    <- solve(B_M, y) ##### (a) TODO #####
a_poly <- solve(B_poly, y) ##### (a) TODO #####

# --- Plot functions ---

plot_M <- function(x_new, n, a_M) {
  B_M_new <- outer(x_new, 0:(n - 1), "^")
  y_new <- c(B_M_new %*% a_M) ##### (b) TODO #####
  plot(x_new, y_new, type = "l")
}

plot_poly <- function(x_new, n, a_poly, B_poly_0) {
  B_poly_new <- cbind(rep(1, length(x_new)),
                      poly(x_new, degree = (n - 1),
                           coefs = attr(B_poly_0, "coefs")))
  y_new <- c(B_poly_new %*% a_poly) ##### (b) TODO #####
  plot(x_new, y_new, type = "l")
}

# --- Evaluate and plot ---

x_new <- seq(0, 1, by = 0.01)

plot_M(x_new, n, a_M)
points(x, y)

plot_poly(x_new, n, a_poly, B_poly_0)
points(x, y)

# (c) No difference expected. Both bases span the same polynomial space
#     (degree <= n - 1), so the interpolating polynomial is unique. The
#     orthogonal basis only improves the conditioning of the linear system,
#     not the result. a_M and a_poly represent the same function in
#     different coordinates.

# (d) With n = 20, the monomial basis matrix B_M becomes computationally
#     singular: for x in [0,1], columns x ^ k and x ^ (k + 1) become nearly
#     indistinguishable for large k, so solve(B_M, y) fails.
#     The orthogonal basis avoids this by construction (orthogonal columns).
#     However, a new problem appears: the degree-19 polynomial oscillates
#     wildly near the interval boundaries (Runge's phenomenon).
#     This motivates the use of local piecewise polynomials (splines).

set.seed(2)
n <- 20
x <- seq(0, 1, length = n)
y <- runif(n, - 1, 1)

B_M <- outer(x, 0:(n - 1), "^")
B_poly_0 <- poly(x, degree = (n - 1))
B_poly <- cbind(rep(1, n), B_poly_0)

# a_M <- solve(B_M, y)          # fails: system is computationally singular
a_poly <- solve(B_poly, y)

x_new <- seq(0, 1, by = 0.01)
plot_poly(x_new, n, a_poly, B_poly_0)
points(x, y)

# --- Condition number comparison ---
kappa(outer(seq(0, 1, length = 5), 0:4, "^")) # [1] 779.375
kappa(cbind(1, poly(seq(0, 1, length = 5), degree = 4))) # [1] 2.111768
kappa(outer(seq(0, 1, length = 20), 0:19, "^")) # [1] 1.819788e+16
kappa(cbind(1, poly(seq(0, 1, length = 20), degree = 19))) # [1] 4.431748
