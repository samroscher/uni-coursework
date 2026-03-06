# advanced mathematical methods
# sheet 7 exercise 3
# Topics: Multilinear maps, tensor products, Kronecker products

# In R, tensors can be stored as arrays.
# e.g. array(1:24, dim = c(2, 3, 4)) creates a rank-3 tensor in R^{2x3x4}
# Entries of a tensor can be accessed like a matrix:
# tensor[1, 1, 1] returns a single entry
# tensor[1, , ] returns a matrix
# We want to implement the tensor product of a matrix and a vector,
# examine it more closely, and explore its relationship to the Kronecker product.

# Task (a):
# Implement a function tensor_product(A, b) that computes the tensor product
# A ⊗ b ∈ R^{k×l×m} between a matrix A ∈ R^{k×l} and a vector b ∈ R^m.

# The tensor product multiplies every entry a_{i, j} of A with every entry b_n of b,
# producing a new tensor c_{i, j, n} = a_{i, j} * b_n.
# Geometrically: each slice [, , n] is a scaled copy of A, scaled by b[n].

# A: matrix of dimension k x l
# b: vector of length m
# returns: array of dimension k x l x m, where slice [, , n] = A * b[n]

tensor_product <- function(A, b) {
  k <- nrow(A)
  l <- ncol(A)
  m <- length(b)
  tensor <- array(dim = c(k, l, m))

  for (i in seq_len(m)) {
    tensor[, , i] <- A * b[[i]]
  }

  tensor
}

# --- Verification ---
B <- matrix(1:6, nrow = 2, ncol = 3)
x <- 1:4
tensor_product(B, x)

# Task (b):
# Implement a function tensor_product2(b, A) that computes the tensor
# product b ⊗ A ∈ R^{k×l×m} between a vector b ∈ R^k and a matrix A ∈ R^{l×m}.

# b: vector of length k
# A: matrix of dimension l x m
# returns: array of dimension k x l x m, where slice [i, , ] = A * b[i]

tensor_product2 <- function(b, A) {
  k <- length(b)
  l <- nrow(A)
  m <- ncol(A)
  tensor <- array(dim = c(k, l, m))

  for (i in seq_len(k)) {
    tensor[i, , ] <- b[[i]] * A
  }

  tensor
}

# --- Verification ---
tensor_product2(x, B)

# Task (c):
# Test tensor_product() and tensor_product2() using three random vectors
# x1, x2, x3. The tensor product x1 ⊗ x2 ⊗ x3 can be written as
# (x1 x2^T) ⊗ x3 or as x1 ⊗ (x2 x3^T).
# Verify that tensor_product(x1 %*% t(x2), x3) equals tensor_product2(x1, x2 %*% t(x3)).

set.seed(1)
x1 <- sample.int(n = 10, size = 3, replace = TRUE)
x2 <- sample.int(n = 10, size = 2, replace = TRUE)
x3 <- sample.int(n = 10, size = 4, replace = TRUE)

x1; x2; x3

tensor1 <- tensor_product(x1 %*% t(x2), x3)
tensor2 <- tensor_product2(x1, x2 %*% t(x3))

all.equal(tensor1, tensor2)

# Task (d):
# Implement a function flatten_tensor that reshapes a tensor
# C ∈ R^{k×l×m} into a matrix ∈ R^{km×l} by stacking the k×l matrices
# C[, , i] on top of each other (i = 1, ..., m).
# Verify: flatten_tensor(tensor_product(A, b)) should equal kronecker(b, A).

flatten_tensor <- function(C) {
  k <- dim(C)[[1]]
  l <- dim(C)[[2]]
  m <- dim(C)[[3]]
  mat <- matrix(nrow = k * m, ncol = l)
  for (i in seq_len(m)) {
    rows <- ((i - 1) * k + 1):(i * k)
    mat[rows, ] <- C[, , i]
  }
  mat
}

# --- Verification ---
C <- tensor_product(B, x)
C_flat <- flatten_tensor(C)
all.equal(C_flat, kronecker(x, B))
