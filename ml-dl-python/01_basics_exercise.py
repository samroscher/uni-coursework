"""
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 1: Basics
"""

x = [1, 2, 3, 4, 5]

#%% ------------------------------------------------------------------------------------
# EX01: Extract the first 3 entries from list x
print('---------- EX-01 ----------')

sub_list = x[:3]

print(sub_list)

#%% ------------------------------------------------------------------------------------
# EX02: Extract the last 3 entries from list x
print('---------- EX-02 ----------')

sub_list = x[-3:]

print(sub_list)

#%% ------------------------------------------------------------------------------------
# EX03: Extract all entries from x except the last one
print('---------- EX-03 ----------')

sub_list = x[:-1]

print(sub_list)

#%% ------------------------------------------------------------------------------------
# EX04: Extract the word 'hello' from the list below
print('---------- EX-04 ----------')

lst = [1, 2, [3, 4], [5, [100, 200, ['hello']], 23, 11], 1, 7]

var = lst[3][1][2][0]

print(var)

#%% ------------------------------------------------------------------------------------
# EX05: Extract the word 'hard' from the the following - badly formatted - dictionary
print('---------- EX-05 ----------')

d = {'oh': [1, 2, 3, {'man': ['why', 'is', 'this', {'so': [1, 2, 3, 'hard']}]}]}

var = d["oh"][3]["man"][3]["so"][3]

print(var)

#%% ------------------------------------------------------------------------------------
# EX06: Split the following string by space into a list of three elements
print('---------- EX-06 ----------')

s = 'Hello dear students!'

str_list = s.split()

print(str_list)

#%% ------------------------------------------------------------------------------------
# EX07: Write a function `contains_ball` that returns True if its given input includes the word 'ball'
print('---------- EX-07 ----------')

def contains_ball(sentence):
    words = sentence.split()
    words = [(word).lower().strip(".,!?") for word in words]
    return "ball" in words

try:
    result = contains_ball('Is there a ball in the room here?')
    print(result)

    assert result, 'Your method does not return the correct result.'
except NameError:
    print('Seems like there is no "contains_ball" function.')


#%% ------------------------------------------------------------------------------------
# EX08: Write a function `count_ball` that counts the number of times
# the word 'ball' appears in a string.
print('---------- EX-08 ----------')

def count_ball(string):
    words = string.split()
    count = 0
    for i in words:
        if i.lower().strip(".,!?") == "ball":
            count += 1
    return count


try:
    result = count_ball('Is there a ball in the room here? Yes, a ball there is.')
    print(result)

    assert result == 2, 'Your method does not return the correct result.'
except NameError:
    print('Seems like there is no "count_ball" function.')

#%% ------------------------------------------------------------------------------------
# EX09: Write a function that reads a CSV file
print('---------- EX-09 ----------')

# For this we will use the wine quality dataset, which we will also use in later exercises
# Please download it from the UCI repository and put it in the same directory as the exercise
# 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'

dataset = []

# The function open() opens a file in read mode. The file remains open in the following
# code block and is closed automatically when leaving the code block.
with open('files/winequality-red.csv') as open_file:
    # You can iterate over the elements, which are the lines of the file
    for i, line in enumerate(open_file):
        # The following line removes whitespace around the string and replaces all
        # double quotes by an empty string (i.e., removes the double quotes)
        if i != 0:
            line = line.strip().replace('"', '').split(";")
            line = [float(x) for x in line]
            line[-1] = int(line[-1])
            dataset.append(line)
        else:
            line = line.strip().replace('"', '').split(";")
            dataset.append(line)

        # Your task is to convert the string into a list of individual elements
        # and cast these to the respective data type (float, except for quality, which is int)

print(dataset[:3])

#%% ------------------------------------------------------------------------------------
# EX10: Finally, convert the dataset in a dictionary of dictionaries, so that we can access
# individual elements of the dataset as 'datasets[17]['quality'], which would return the
# quality of the 17th row (starting at 1)
print('---------- EX-10 ----------')

dataset_as_dict = {}
header = dataset[0]

for entry, values in enumerate(dataset[1:], start=1):
    row = {}
    for column, value in enumerate(values):
        key = header[column]
        row[key] = value
    dataset_as_dict[entry] = row


result = dataset_as_dict[17]['quality']
print(result)

assert result == 7, result
assert isinstance(result, int)

# %%
