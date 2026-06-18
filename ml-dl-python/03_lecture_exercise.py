'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 3: Pandas Lecture
'''

import pandas as pd
import numpy as np
from numpy.random import randn

#%% ------------------------------------------------------------------------------------
# BLOCK 1: Selecting Specific Rows & Columns
# ------------------------------------------

print('#'*50)
print('########## Selecting Specific Rows & Columns ##########')
print('#'*50)

# Set a seed for reproducible random numbers
np.random.seed(101)

# Consider the following data frame:
df = pd.DataFrame (data = randn(3,4),
                   index = 'A B C'.split(),
                   columns = 'W X Y Z'.split())

#%% ------------------------------------------------------------------------------------
# EX01: select the second row by number and the third colmun by number
print('---------- EX-01 ----------')
df.iloc[1, 2]

#%% ------------------------------------------------------------------------------------
# EX02: select the second row by name and the third colmun by name
print('---------- EX-02 ----------')
df.loc['B', 'Y']

#%% ------------------------------------------------------------------------------------
# EX03: select the second row by number and the third colmun by name
print('---------- EX-03 ----------')
df.loc[df.index[1], 'Y']

#%% ------------------------------------------------------------------------------------
# EX04: select the second row by name and the third colmun by number
print('---------- EX-04 ----------')
df.loc['B', df.columns[2]]
