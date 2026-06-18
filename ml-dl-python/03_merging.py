'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Lecture 3: Merging
'''

### Merging etc.

import pandas as pd

###############################################################################
### a) concat()

# Here DataFrames are simply appended to each other, like the rbind() and cbind() in R. 
# The function is pd.concat(axis = 0), with the default value of axis = 0 => lines are appended to each other (like rbind)

# Create DataFrames
df1 = pd.DataFrame({'A': ['A0', 'A1', 'A2', 'A3'],
                    'B': ['B0', 'B1', 'B2', 'B3'],
                    'C': ['C0', 'C1', 'C2', 'C3'],
                    'D': ['D0', 'D1', 'D2', 'D3']},
                        index=[0, 1, 2, 3])
df2 = pd.DataFrame({'A': ['A4', 'A5', 'A6', 'A7'],
                    'B': ['B4', 'B5', 'B6', 'B7'],
                    'C': ['C4', 'C5', 'C6', 'C7'],
                    'D': ['D4', 'D5', 'D6', 'D7']},
                         index=[4, 5, 6, 7])
df3 = pd.DataFrame({'A': ['A8', 'A9', 'A10', 'A11'],
                    'B': ['B8', 'B9', 'B10', 'B11'],
                    'C': ['C8', 'C9', 'C10', 'C11'],
                    'D': ['D8', 'D9', 'D10', 'D11']},
                        index=[8, 9, 10, 11])
df1
df2
df3

# Concatenate
pd.concat([df1, df2, df3])

# Side by side: Warning, it is done based on the index number and not the line number (unlike cbind()).

pd.concat([df1, df2, df3],
          axis = 1)
# Here, the concatenation is done based on the column names at axis = 0.
# It didn't attract attention because each of them has the same name and the same order => So be careful here.

###############################################################################
### b) merge()

# Create DataFrames
left = pd.DataFrame({'key' : ['K0', 'K1', 'K2', 'K3'],
                     'A'   : ['A0', 'A1', 'A2', 'A3'],
                     'B'   : ['B0', 'B1', 'B2', 'B3']})
   
right = pd.DataFrame({'key': ['K0', 'K1', 'K2', 'K3'],
                      'C'  : ['C0', 'C1', 'C2', 'C3'],
                      'D'  : ['D0', 'D1', 'D2', 'D3']})

left
right

# Inner-Join
pd.merge(left,
         right,
         how = 'inner',
         on = 'key')


# Example with two key columns
left = pd.DataFrame({'key1' : ['K0', 'K0', 'K1', 'K2'],
                     'key2' : ['K0', 'K1', 'K0', 'K1'],
                     'A'    : ['A0', 'A1', 'A2', 'A3'],
                     'B'    : ['B0', 'B1', 'B2', 'B3']})
    
right = pd.DataFrame({'key1': ['K0', 'K1', 'K1', 'K2'],
                      'key2': ['K0', 'K0', 'K0', 'K0'],
                      'C'   : ['C0', 'C1', 'C2', 'C3'],
                      'D'   : ['D0', 'D1', 'D2', 'D3']})

# Inner = default
pd.merge(left, right, on = ['key1', 'key2'])

# Outer
pd.merge(left, right, how = 'outer', on = ['key1', 'key2'])

# Right
pd.merge(left, right, how = 'right', on = ['key1', 'key2'])

# Left
pd.merge(left, right, how = 'left', on = ['key1', 'key2'])

###############################################################################
### c) join()
# join is very similar to merge().
# It is essentially a wrapper,  if you want to join dataframes based on indices. See also:
#      https://stackoverflow.com/questions/22676081/what-is-the-difference-between-join-and-merge-in-pandas
#   and
#      https://pandas.pydata.org/pandas-docs/stable/user_guide/merging.html


left = pd.DataFrame({'A': ['A0', 'A1', 'A2'],
                     'B': ['B0', 'B1', 'B2']},
                      index=['K0', 'K1', 'K2'])

right = pd.DataFrame({'C': ['C0', 'C2', 'C3'],
                      'D': ['D0', 'D2', 'D3']},
                      index=['K0', 'K2', 'K3'])

# Left join = default
left.join(right)

left.join(right, how='outer')
