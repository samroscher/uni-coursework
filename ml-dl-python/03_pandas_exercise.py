'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 3: Pandas
'''

import numpy as np
import pandas as pd

'''
The dataset that we will look at in this and the next few exercises contains information about different types of wine.
A detailed description can be found at:
https://archive.ics.uci.edu/ml/datasets/Wine+Quality

In addition to some physical variables such as the alcohol content and 
pH value, the 'quality' column shows the wine quality as an average 
subjective assessment by at least three wine experts. In the next exercise, 
the quality of the wine is to be predicted based on the physical 
measured variables using various machine learning methods.
In this exercise, we get to know the dataset and practice the syntax and functionality of the pandas package.
'''
#%% ------------------------------------------------------------------------------------
# BLOCK 1: Reading Dataset
# ------------------------
print('#'*50)
print('########## Reading Dataset ##########')
print('#'*50)

# Read the dataset from the URL via the following command
# 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
red_url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
red = pd.read_csv(red_url, sep=';')

# Use the following command to output the number of rows and columns in the dataset
n_rows, n_cols = red.shape
print(n_rows, n_cols)

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Selecting Specific Rows/Columns
# ----------------------------------------
print('#'*50)
print('########## Selecting Specific Rows/Columns ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Display the 'alcohol' column
print('---------- EX-01 ----------')
red.loc[:, 'alcohol']

#%% ------------------------------------------------------------------------------------
# EX02: Display the first column
print('---------- EX-02 ----------')
red.iloc[:, 0]

#%% ------------------------------------------------------------------------------------
# EX03: Display the first 5 values of the last column
print('---------- EX-03 ----------')
red.iloc[0:5, -1]

#%% ------------------------------------------------------------------------------------
# EX04: Display all values belonging to the row with index 1
print('---------- EX-04 ----------')
red.loc[1, :]

#%% ------------------------------------------------------------------------------------
# EX05: Display the first 5 values of the quality column
print('---------- EX-05 ----------')
red.loc[red.index[0:5], 'quality']

#%% ------------------------------------------------------------------------------------
# BLOCK 3: Conditional Selection of Rows
# --------------------------------------
print('#'*50)
print('########## Conditional Selection of Rows ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Display all values of all wines with an alcohol content of 14.9
print('---------- EX-01 ----------')
red[red['alcohol'] == 14.9]

#%% ------------------------------------------------------------------------------------
# EX02: Display the alcohol content of all wines whose quality is 3
print('---------- EX-02 ----------')
red.loc[red['quality'] == 3, 'alcohol']

#%% ------------------------------------------------------------------------------------
# EX03: Display all values of all wines whose 'density' is greater than 0.999 and whose value for 'chlorides' is less than 0.065
print('---------- EX-03 ----------')
red[(red['density'] > 0.999) & (red['chlorides'] < 0.065)]

#%% ------------------------------------------------------------------------------------
# BLOCK 4: Adding New Columns
# ---------------------------
print('#'*50)
print('########## Adding New Columns ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Create a new column in which the quality of the wine is binarized as follows:
#   quality >= 6    => 1
#   otherwise       => 0
print('---------- EX-01 ----------')
new_col = (red['quality'] >= 6).astype(int)
red['quality_bin'] = new_col

#%% ------------------------------------------------------------------------------------
# BLOCK 5: Deleting Rows/Columns
# ------------------------------
print('#'*50)
print('########## Deleting Rows/Columns ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Delete the column 'free sulfur dioxide' and look at the result
print('---------- EX-01 ----------')
del red['free sulfur dioxide']
red.head()

#%% ------------------------------------------------------------------------------------
# EX02: Delete the first 5 lines - but not inplace!
print('---------- EX-02 ----------')
red.drop(red.index[0:5], axis=0)

#%% ------------------------------------------------------------------------------------
# EX03: Delete all lines where quality == 3 - but not inplace!
print('---------- EX-03 ----------')
idx = red[red['quality'] == 3].index
red.drop(idx)

#%% ------------------------------------------------------------------------------------
# BLOCK 6: General Overview of Dataset
# ------------------------------------
print('#'*50)
print('########## General Overview of Dataset ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Provide a rough overview of the column names, the number of missing values, and the data types of each column
print('---------- EX-01 ----------')
red.info()

#%% ------------------------------------------------------------------------------------
# EX02: Display the first lines of the dataset to see all columns you have to adjust the pandas options: 
# pd.set_option('display.max_columns', None)
print('---------- EX-02 ----------')
pd.set_option('display.max_columns', None)
red.head()

#%% ------------------------------------------------------------------------------------
# EX03: Give a rough description of all columns
print('---------- EX-03 ----------')
red.describe()
red.isna().sum()

#%% ------------------------------------------------------------------------------------
# EX04: Print the column names
print('---------- EX-04 ----------')
red.columns

#%% ------------------------------------------------------------------------------------
# BLOCK 7: Overview of Target Variables
# -------------------------------------
print('#'*50)
print('########## Overview of Target Variables ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: How many different values does the variable 'quality' have?
print('---------- EX-01 ----------')
n_vals = red['quality'].nunique()
print(n_vals)

#%% ------------------------------------------------------------------------------------
# EX02: Print a frequency table for the variable 'quality', sorted by quality, not the frequency of occurrence
print('---------- EX-02 ----------')
freq_table = red['quality'].value_counts(sort=False).sort_index()
print(freq_table)

#%% ------------------------------------------------------------------------------------
# EX03: What is the data type of quality?
print('---------- EX-03 ----------')
answer = red['quality'].dtype
print(answer)

#%% ------------------------------------------------------------------------------------
# EX-additional: Search on the internet about making a bar plot; create a bar plot of the frequencies for the variable quality
print('---------- EX-additional ----------')
freq_table.plot.bar()

#%% ------------------------------------------------------------------------------------
# BLOCK 8: Missing Values
# -----------------------
print('#'*50)
print('########## Missing Values ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Are there any missing values in the dataset?
print('---------- EX-01 ----------')

answer = red.isna().any().any()
print(answer)
n_missings = red.isna().sum().sum()
print(n_missings)

#%% ------------------------------------------------------------------------------------
# BLOCK 9: Reset Index
# --------------------
print('#'*50)
print('########## Reset Index ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Display the index of the dataframe
print('---------- EX-01 ----------')
red.index

#%% ------------------------------------------------------------------------------------
# EX02: Set 'quality' as the new index - inplace
print('---------- EX-02 ----------')
red.set_index('quality', inplace=True)

#%% ------------------------------------------------------------------------------------
# EX03: Display the first few lines
print('---------- EX-03 ----------')
red.head()

#%% ------------------------------------------------------------------------------------
# EX04: Reset the index and keep the old index so that quality becomes a normal column again
print('---------- EX-04 ----------')
red.reset_index(inplace=True)

#%% ------------------------------------------------------------------------------------
# EX05: Did that work? Did you notice something?
print('---------- EX-05 ----------')
answer = 'Yes, the order of the columns is different than before: Now, quality is the first column.'
print(answer)

#%% ------------------------------------------------------------------------------------
# BLOCK 10: Converting Columns Units
# ----------------------------------
print('#'*50)
print('########## Converting Columns Units ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Convert 'citric acid' into whole number percentages and define the data type to be integer
print('---------- EX-01 ----------')
red['citric acid pct'] = (red['citric acid'] * 100).astype(int)
del red['citric acid']

#%% ------------------------------------------------------------------------------------
# BLOCK 11: Sum/Mean of Columns/Rows 
# ----------------------------------
print('#'*50)
print('########## Sum/Mean of Columns/Rows ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Calculate the column sums and means
print('---------- EX-01 ----------')

res_sums = red.sum()
print(res_sums)
res_means = red.mean()
print(res_means)

#%% ------------------------------------------------------------------------------------
# BLOCK 12: Grouping
# ------------------
print('#'*50)
print('########## Grouping ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Calculate the average and maximum alcohol content for each quality level
print('---------- EX-01 ----------')

res_avg = red.groupby(['quality'])['alcohol'].mean()
print(res_avg)
res_max = red.groupby(['quality'])['alcohol'].max()
print(res_max)

#%% ------------------------------------------------------------------------------------
# EX-additional: Calculate the average quality level per 'citric acid_100'; present the results in a scatter plot
print('---------- EX-additional ----------')
red.groupby(['citric acid pct'])['quality'].mean().plot(style='.')

#%% ------------------------------------------------------------------------------------
# BLOCK 13: Sorting
# -----------------
print('#'*50)
print('########## Sorting ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Sort the dataset in descending order of quality
print('---------- EX-01 ----------')
red.sort_values(by='quality', ascending=False)

#%% ------------------------------------------------------------------------------------
# BLOCK 14: Masking 
# -----------------
print('#'*50)
print('########## Masking ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Replace all quality levels = 3 with the value 300
print('---------- EX-01 ----------')
red.loc[red['quality'] == 3, 'quality'] = 300

#%% ------------------------------------------------------------------------------------
# EX02: Replace all quality levels = 300 with NAs (np.nan)
print('---------- EX-02 ----------')
red.loc[red['quality'] == 300, 'quality'] = np.nan

#%% ------------------------------------------------------------------------------------
# EX03: How many missing values are in 'quality' now?
print('---------- EX-03 ----------')
n_missings = red['quality'].isna().sum()
print(n_missings)

#%% ------------------------------------------------------------------------------------
# EX04: Fill in the missing values in 'quality' with the column median
print('---------- EX-04 ----------')
red['quality'] = red['quality'].fillna(red['quality'].median())

#%% ------------------------------------------------------------------------------------
# BLOCK 15: Saving Dataset
# ------------------------
print('#'*50)
print('########## Saving Dataset ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Save the dataset as a csv file
print('---------- EX-01 ----------')
red.to_csv('../data/red.csv')