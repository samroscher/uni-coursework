"""
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 2: Numpy
"""

import numpy as np

#%% ------------------------------------------------------------------------------------
# BLOCK 1: ARRAY CREATION
# -----------------------
print('#'*50)
print('########## ARRAY CREATION ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Create a NumPy array with 10 zeros
print('---------- EX-01 ----------')

arr = np.zeros((10,))

print(arr)

#%% ------------------------------------------------------------------------------------
# EX02: Create a NumPy array with 10 ones
print('---------- EX-02 ----------')

arr = np.ones((10,))

print(arr)

#%% ------------------------------------------------------------------------------------
# EX03: Create a NumPy array with 10 fives
print('---------- EX-03 ----------')

arr = np.full((10,), 5)

print(arr)

#%% ------------------------------------------------------------------------------------
# EX04: Create a NumPy array with the integers from 10 to 50
print('---------- EX-04 ----------')

arr = np.arange(10, 51)

print(arr)

#%% ------------------------------------------------------------------------------------
# EX05: Create a NumPy array with the even numbers from 10 to 50
print('---------- EX-05 ----------')

arr = arr[arr % 2 == 0]

print(arr)

#%% ------------------------------------------------------------------------------------
# EX06: Create a 3x3 identity matrix
print('---------- EX-06 ----------')

arr = np.eye(3)

print(arr)

#%% ------------------------------------------------------------------------------------
# EX07: Create a NumPy array with 20 equidistant points
# between 0 and 1 (including the endpoints)
print('---------- EX-07 ----------')

arr = np.linspace(0, 1, 20)

print(arr)

#%% ------------------------------------------------------------------------------------
# BLOCK 2: RANDOM NUMBERS
# -----------------------
print('#'*50)
print('########## RANDOM NUMBERS ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Draw a random number from the uniform distribution [0, 1)
print('---------- EX-01 ----------')

rng = np.random.default_rng(seed=123)
num = rng.uniform()
print(num)


#%% ------------------------------------------------------------------------------------
# EX02: Draw 25 random numbers from the N(0, 1) distribution
print('---------- EX-02 ----------')

num = rng.standard_normal(size=(25,))

print(num)

#%% ------------------------------------------------------------------------------------
# EX03: Draw 5 random integers between 2 and 10
print('---------- EX-03 ----------')

num = rng.integers(2, 11, (5,))

print(num)


#%% ------------------------------------------------------------------------------------
# BLOCK 3: ATTRIBUTES & METHODS
# -----------------------------
print('#'*50)
print('########## ATTRIBUTES & METHODS ##########')
print('#'*50)


#%% ------------------------------------------------------------------------------------
# EX01: Randomly reorder the array rows. The row values should always stay together.
print('---------- EX-01 ----------')
# Given this 3x3 matrix:
arr = np.arange(9).reshape(3, 3)
rng.shuffle(arr)

print(arr)

#%% ------------------------------------------------------------------------------------
# EX02: Create a 10x10 matrix with values ranging from 0.01 to 1 with step size 0.01
print('---------- EX-02 ----------')

arr = np.linspace(0.01, 1,100).reshape((10, 10))

print(arr)


#%% ------------------------------------------------------------------------------------
# BLOCK 4: INDEXING & SLICING
# ---------------------------
print('#'*50)
print('########## INDEXING & SLICING ##########')
print('#'*50)

# The following matrix is given:
mat = np.arange(1,26).reshape(5,5)

print('-- GIVEN MATRIX:')
print(mat)

#%% ------------------------------------------------------------------------------------
# EX01: Extract the 3x3 matrix in the lower right corner
print('---------- EX-01 ----------')

arr = mat[-3:, -3:]

print(arr)

#%% ------------------------------------------------------------------------------------
# EX02: Access the value 20
print('---------- EX-02 ----------')

arr = mat[3, 4]

print(arr)

#%% ------------------------------------------------------------------------------------
# EX03: Extract the first 3 values of the 2nd column
print('---------- EX-03 ----------')

arr = mat[:3,1]

print(arr)

#%% ------------------------------------------------------------------------------------
# EX04: Extract the 5th row
print('---------- EX-04 ----------')

arr = mat[4,:]

print(arr)

#%% ------------------------------------------------------------------------------------
# EX05: Extract all even numbers
print('---------- EX-05 ----------')

arr = mat[mat % 2 == 0]

print(arr)

#%% ------------------------------------------------------------------------------------
# BLOCK 5: ARITHMETIC OPERATIONS
# ------------------------------
print('#'*50)
print('########## ARITHMETIC OPERATIONS ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Calculate the sum of all the mat values
print('---------- EX-01 ----------')

res = np.sum(mat)

print(res)

#%% ------------------------------------------------------------------------------------
# EX02: Calculate the empirical standard deviation of the mat values
print('---------- EX-02 ----------')

res = np.std(mat, ddof=1)

print(res)

#%% ------------------------------------------------------------------------------------
# EX03: Calculate the sum of the mat columns.
print('---------- EX-03 ----------')

res = np.sum(mat, axis=(0,))

print(res)

#%% ------------------------------------------------------------------------------------
# EX04: Double all the mat values
print('---------- EX-04 ----------')

res = mat * 2

print(res)

#%% ------------------------------------------------------------------------------------
# BLOCK 6: Loading data files (advanced)
# --------------------------------------

#%% ------------------------------------------------------------------------------------
# EX01: Python's csv module
# Now that we have learned how to import modules, we can make use of Python's
# built-in csv reader. Below you can see two ways to use it. Use one of these
# to read the file
# 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
# from last week into a numpy array
print('---------- EX-01 ----------')

import csv
with open("files/winequality-red.csv") as csv_file:
    reader = csv.DictReader(csv_file, delimiter=";")
    for row in reader:
        print(row)
        break

rows = []
with open("files/winequality-red.csv") as csv_file:
    reader = csv.reader(csv_file, delimiter=";")
    header = next(reader)
    for row in reader:
        rows.append(row)
    dataset = np.array(rows, float)

print(dataset)

#%% ------------------------------------------------------------------------------------
# EX02: Loading the file with numpy's built-in loader
# Numpy provides multiple loaders. Check out np.loadtxt and configure its arguments
# to load the file at hand

dataset_via_np = np.loadtxt("files/winequality-red.csv", float, delimiter=";", skiprows=1)

print(dataset_via_np)

# This next line check that the dataset read manually from the CSV and
# via numpy's loader are the same
np.testing.assert_array_almost_equal(dataset, dataset_via_np)

#%% ------------------------------------------------------------------------------------
# BLOCK 6: Some Data Science
# --------------------------

#%% ------------------------------------------------------------------------------------
# EX01: Calculate the average rating of the wines

quality = np.mean(dataset[:,-1])

print(quality)
np.testing.assert_almost_equal(quality, 5.63602251407)

#%% ------------------------------------------------------------------------------------
# EX02: Find all unique ratings
# Hint: check the numpy documentation for a function that provides this information
unique = np.unique(dataset[:,-1])
print(unique)

#%% ------------------------------------------------------------------------------------
# EX03: Extract all entries with the highest rating
subset = dataset[dataset[:, -1] == dataset[:, -1].max(), :]
print(subset.shape)
assert subset.shape == (18, 12), subset.shape

#%% ------------------------------------------------------------------------------------
# EX04: Check if there are any NaNs in the dataset
print(np.any(np.isnan(dataset_via_np)))

#%% ------------------------------------------------------------------------------------
# BLOCK 7: Linear Regression
# --------------------------

#%% ------------------------------------------------------------------------------------
# EX01: Split the dataset into X and y
X = dataset[:, :-1]
y = dataset[:, -1]

#%% ------------------------------------------------------------------------------------
# EX02: Advanced Numpy: Least-Squares Regression
# Numpy provides many linear algebra routines in the subpackage np.linalg
# Find the appropriate function and obtain the coefficients `beta`
beta, _, _, _ = np.linalg.lstsq(X, y, )

#%% ------------------------------------------------------------------------------------
# EX02: Predictions
# Compute the predictions as $X \beta$
y_hat = X @ beta

#%% ------------------------------------------------------------------------------------
# EX03: Compute the root mean squared error to judge the quality of the solution
rmse = np.sqrt(np.mean((y - y_hat)**2))
print(rmse)

np.testing.assert_almost_equal(rmse, 0.6457934846531704)
