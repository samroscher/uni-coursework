'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 6: NLP Basics / RegEx
'''

import pandas as pd
import numpy as np
import re
import nltk
nltk.download('punkt') # for the tokenization
nltk.download('stopwords')
import urllib.request

#%% ------------------------------------------------------------------------------------
# BLOCK 1: Reading and Displaying Images
# --------------------------------------
print('#'*50)
print('########## Reading the data ##########')
print('#'*50)

data = urllib.request.urlopen('https://raw.githubusercontent.com/ryanmcdermott/trump-speeches/master/speeches.txt')
speeches = [line.decode('utf-8') for line in data]
speeches = " ".join(speeches)

#%% ------------------------------------------------------------------------------------
# EX01: Inspect the data: What is the format? How do we handle it?
print('---------- EX-01 ----------')
 
speeches_sub = speeches[:1000]
print(speeches_sub)

#%% ------------------------------------------------------------------------------------
# EX02: Can we find out how many speeches there are in our data set? (Hint: Use regex patterns in a clever way)
print('---------- EX-02 ----------')

pattern = r'SPEECH \d+'
number_speeches = len(re.findall(pattern, speeches))
print(number_speeches)

#%% ------------------------------------------------------------------------------------
# EX03: Split up the data into the different speeches
print('---------- EX-03 ----------')

list_speeches = re.split(pattern, speeches)
len(list_speeches)

#%% ------------------------------------------------------------------------------------
# EX04: Display the beginning of each speech
# (Hint: Use a list comprehension)
print('---------- EX-04 ----------')

beginnings = [x[:500] for x in list_speeches]
print(beginnings)

#%% ------------------------------------------------------------------------------------
# EX05: Extract just the first speech from the list of speeches
# (Hint: The first one is at postion index 1, since the one at index 0 is some garbage)
print('---------- EX-05 ----------')

speech = list_speeches[1]
print(speech[:300])

#%% ------------------------------------------------------------------------------------
# EX06: Transform the whole speech to lowercase
print('---------- EX-06 ----------')

speech = speech.lower()
print(speech[:300])

#%% ------------------------------------------------------------------------------------
# EX07: Use a regex pattern to find all contractions in the speech
# (Hint: You can use sorted() to get the contractions in the same order as below)

pattern = r'\w+\'\w+'
contractions = sorted(set(re.findall(pattern, speech)))
print(contractions)

# Here is a list of the expanded contractions for replacing the them in the next step
expanded = ['are not', 'can not', 'countrys', 'did not', 'does not', 'do not', 'everyone is', 'he is', 'i will', 'i am', 'i have', 'is not', 'it is', 'that is', 
            'they would', 'they are', 'they have', 'we will', 'we are', 'we have', 'were not', 'what is', 'who is', 'will not', 'would not', 'you would', 
            'you will', 'you are', 'you have']

#%% ------------------------------------------------------------------------------------
# EX08: Use a for-loop to replace each contraction by its expanded version

for word in range(len(contractions)):
    speech = re.sub(contractions[word], expanded[word], speech)
print(speech[:100])

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Tokenization
# ---------------------
print('#'*50)
print('########## Tokenizing text ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Tokenize that speech
print('---------- EX-01 ----------')

speech = nltk.word_tokenize(speech)
print(speech[:50])

#%% ------------------------------------------------------------------------------------
# Assume we have the following stopword list
from nltk.corpus import stopwords
stopwords = stopwords.words("english")

#%% ------------------------------------------------------------------------------------
# EX02: Remove these stopwords from the speech
print('---------- EX-02 ----------')

speech = [word for word in speech if word not in stopwords]
print(speech[:50])

#%% ------------------------------------------------------------------------------------
# EX03: Remove the unwanted tokens from the speech
# Unwanted tokens are in this case the following ones: .,;:!?\$\-'`?
print('---------- EX-03 ----------')

speech = re.sub(r'[.,;:!?$\-\'`]', '', ' '.join(speech))
print(speech[:50])

#%% ------------------------------------------------------------------------------------
# EX04: Remove numbers from the speech
print('---------- EX-04 ----------')

speech = re.sub(r'\d+', '', speech)
print(speech[:50])
