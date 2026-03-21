# advanced mathematical methods
# sheet 04 exercise 3
# Topics: SVD, truncated SVD, rank-k approximation, image compression,
#         spectral norm approximation error

# A matrix A of rank r has SVD A = sum_{i=1}^r sigma_i u_i v_i ^ T.
# The best rank-k approximation (in spectral and Frobenius norm) is
#   A_k = sum_{i=1}^k sigma_i u_i v_i ^ T = U_k Sigma_k V_k ^ T.
# (a) shows ||A - A_k||_2 = sigma_{k+1}.

# ---- (a) ----
# A - A_k = sum_{i=k+1}^r sigma_i u_i v_i ^ T, which is itself an SVD
# (orthonormal u's and v's, singular values sigma_{k+1} >= ... >= sigma_r).
# The spectral norm equals the largest singular value, so
#   ||A - A_k||_2 = sigma_{k+1}.

# ---- (b) ----
# Download the Mona Lisa BW image from:
#   https://upload.wikimedia.org/wikipedia/commons/6/6e/Mona_Lisa_bw_square.jpeg
# and place it in your working directory.

library(jpeg)

pic <- readJPEG("Mona_Lisa_bw_square.jpeg")
pic <- apply(pic, 2, rev)

# original image
image(t(pic), col = grey(seq(0, 1, length = 256)))

# SVD
n_pic <- nrow(pic)
svd_pic <- svd(pic)

# singular value decay
plot(1:n_pic, log(svd_pic$d),
     xlab = "index", ylab = "log(singular value)")
plot(1:200, log(svd_pic$d[1:200]),
     xlab = "index", ylab = "log(singular value)")

# rank-k approximations
for (k in c(5, 10, 20, 50, 100, 200)) {
  approx_k <- svd_pic$u[, 1:k] %*%
    diag(svd_pic$d[1:k]) %*%
    t(svd_pic$v[, 1:k])
  image(t(approx_k), col = grey(seq(0, 1, length = 256)))
  n_stored <- 2 * n_pic * k + k
  pct <- round(n_stored / n_pic ^ 2 * 100)
  title(main = paste0("k = ", k,
                      ", stored values: ", n_stored,
                      ", ca ", pct, "% of original"),
        cex.main = 0.5)
}

# ---- Bonus: storage comparison ----
# Original: m * n values.
# Rank-k:   U_k (m x k) + Sigma_k (k diagonal) + V_k (n x k) = (m + n) * k + k.
# Here m = n = 743, so original = 552049.
n_pic ^ 2
# Already at k = 50 (~13%) or k = 100 (~27%) the approximation is visually
# good, using only a fraction of the original storage.