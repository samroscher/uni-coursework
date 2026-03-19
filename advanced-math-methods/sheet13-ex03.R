# advanced mathematical methods
# sheet 13 exercise 3
# Topics: Spline interpolation, B-splines, gamma function approximation,
#         knot placement

# The gamma function Gamma: R+ -> R is defined by the integral:
#   Gamma(x) = integral_0^inf t ^ (x - 1) e ^ (- t) dt.
# In general, the integral has no closed-form solution, but
#   Gamma(n) = (n - 1)! for all n in N.
# We want to approximate Gamma on [1, n] using a degree-q spline
#   s_q(x) = sum_{k = 1}^{m + q} a_k B_k ^ (q) (x)
#   with knots xi_0, ..., xi_m, using the data points (i, (i - 1)!)_{i = 1}^n.

# (a) For given q and n, how large must m be for the linear system to have
#     a unique solution?
# (b) Find a cubic spline interpolating (i, (i - 1)!)_{i = 1}^5 and plot it
#     together with the interpolated points. Use bs() from the splines package.
#     Hint: set the intercept argument to TRUE.
# (c) Compare the spline function with the result of base::gamma().
# (d) Vary the knots argument of bs(). How does the interpolation change?
#     How should a knot sequence be chosen in the best / worst case?

# ---- (a) ----
# The spline has m + q basis functions, so m + q unknowns.
# The n data points give n equations: B * a = y, B in R ^ (n x (m + q)).
# Unique solution requires m + q = n, i.e. m = n - q.

# ---- (b) ----
# Terminology:
#   - df (degrees of freedom) = number of basis functions = number of columns
#     in B. For a spline with m + 1 knots and degree q: df = m + q.
#   - Data points (x_i, y_i): where the spline must hit certain values
#     (interpolation conditions = rows in the linear system).
#   - Knots xi_j: where a new polynomial piece begins (structure of the
#     spline = affects the basis functions = columns in the linear system).
#     Knots do NOT have to coincide with data points.
# Here: n = 5 data points, q = 3 (cubic), so m = n - q = 2, df = m + q = 5.
# The 3 knots are xi_0 = 1 (left boundary), xi_1 (interior), xi_2 = 5
# (right boundary). bs() places the interior knot by default at the median.
# intercept = TRUE includes the constant basis function, so that B is
# 5 x 5 (without it, bs() drops one column and B would be 5 x 4).

q <- 3
n <- 5
x <- 1:n
y <- factorial(x - 1)

B <- splines::bs(x, df = 5, degree = 3, intercept = TRUE)
a <- solve(B, y)

# Evaluate spline at new x values: we must reuse the same basis (same knots,
# same degree) at the new points. We extract knots and boundary knots from
# the original B to ensure consistency.
eval_bs <- function(x_new, basis) {
  splines::bs(
    x = x_new,
    degree = attr(basis, "degree"),
    knots = attr(basis, "knots"),
    Boundary.knots = attr(basis, "Boundary.knots"),
    intercept = attr(basis, "intercept")
  )
}

x_new <- seq(1, 5, length = 100)
y_hat <- eval_bs(x_new, B) %*% a

plot(x_new, y_hat, type = "l")
lines(x_new, gamma(x_new), col = 2) # (c)
points(x, y)
legend("topleft", legend = c("spline", "gamma"), col = c(1, 2), lty = 1)

# ---- (c) ----
# The spline hits all 5 data points exactly (interpolation), but deviates
# from the true gamma curve between points, especially in the steeper
# region [4, 5] where gamma grows rapidly.

# ---- (d) ----
compare_knots <- function(knot) {
  x <- 1:5
  y <- factorial(x - 1)
  B <- splines::bs(x, df = 5, degree = 3, knots = knot, intercept = TRUE)
  a <- solve(B, y)
  x_new <- seq(1, 5, length = 100)
  y_hat <- eval_bs(x_new, B) %*% a
  plot(x_new, y_hat, type = "l")
  lines(x_new, gamma(x_new), col = 2)
  points(x, y)
  legend("topleft", legend = c("spline", "gamma"), col = c(1, 2), lty = 1)
  title(paste("inner knot at", knot))
}

compare_knots(3)
compare_knots(1.5)
compare_knots(4.5)
compare_knots(3.5)

# Since m = 2, the boundary knots xi_0 = 1 and xi_2 = 5 are fixed.
# Only the interior knot xi_1 is free. Observations:
#   - Knots near the boundary (e.g. 1.5 or 4.5) waste flexibility:
#     one interval becomes tiny (over-resolved), the other huge
#     (under-resolved). This creates visible artifacts.
#   - xi_1 = 1.5 is worse than xi_1 = 4.5 because the steep region [4, 5]
#     needs more resolution, so starving it of knots hurts more.
#   - xi_1 = 3 (default, equidistant) works well.
#   - xi_1 = 3.5 is slightly better: gamma is nearly linear on [1, 3]
#     (needs little flexibility) but steeply curved on [3, 5] (needs more).
#     Shifting the knot toward the steeper region improves the fit.
# General rule: place knots where the function changes most.