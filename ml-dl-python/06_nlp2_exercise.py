'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 6: word2vec / doc2vec / fasttext
'''

import pandas as pd
import numpy as np
import re
import nltk
from gensim.models import Word2Vec
from gensim.models.doc2vec import Doc2Vec
from gensim.models.doc2vec import TaggedDocument
from gensim.models.fasttext import FastText
from pprint import pprint

#%% ------------------------------------------------------------------------------------
# BLOCK 1: Inspecting the data + basic preprocessing
# --------------------------------------------------
print('#'*50)
print('########## Reading the data ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Import the data using pandas
print('---------- EX-01 ----------')

tweet_data = pd.read_csv('./files/trump.csv')
tweet_data.head()

#%% ------------------------------------------------------------------------------------
# EX02: Extract the tweets to a list of strings
print('---------- EX-02 ----------')

tweets_raw = [tweet for tweet in tweet_data['text']]
print(tweets_raw[0])

#%% ------------------------------------------------------------------------------------
# EX03: Convert everything to lowercase
print('---------- EX-03 ----------')

tweets = [tweet.lower() for tweet in tweets_raw]
print(tweets[0])

#%% ------------------------------------------------------------------------------------
# EX04: Delete url adresses and other unwanted tokens
# (use one list comprehension for deleting urls, one for the others and one for tokenization)
print('---------- EX-04 ----------')

regex_urls = r"https://.*|"
regex_other = r"[\)\(\.\,;:!?\+\-\_\#\'\*\?\$\%\&]"

tweets = [re.sub(regex_urls, '', tweet) for tweet in tweets]
tweets = [re.sub(regex_other, '', tweet) for tweet in tweets]
tweets = [nltk.tokenize.word_tokenize(tweet) for tweet in tweets]
print(tweets[0])

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Learning word vector representations
# ---------------------------------------------
print('#'*50)
print('########## Word2Vec ##########')
print('#'*50)

# First, we determine the number of CPUs that are available on our machine
# (The more cores are available, the faster we can train our model)

import multiprocessing
cpus = multiprocessing.cpu_count()
print(cpus)

#%% ------------------------------------------------------------------------------------
# EX01: Set up the model (use only the defaults, use all your gpu cores except one)
print('---------- EX-01 ----------')
w2v_model = Word2Vec(workers=(cpus - 6)) # adapt to your needs

#%% ------------------------------------------------------------------------------------
# EX02: Build the vocabulary
print('---------- EX-02 ----------')
w2v_model.build_vocab(corpus_iterable=tweets, update=False)

#%% ------------------------------------------------------------------------------------
# EX03: Train the model
print('---------- EX-03 ----------')
w2v_model.train(corpus_iterable=tweets, total_examples=w2v_model.corpus_count, epochs=100)

#%% ------------------------------------------------------------------------------------
# EX04: Now: Explore your model, e.g.
print('---------- EX-04 ----------')

print(w2v_model.wv.most_similar(positive = ["germany"]))
print(w2v_model.wv.most_similar(positive = ["clinton"]))
print(w2v_model.wv.most_similar(positive = ["democrats"]))
print(w2v_model.wv.most_similar(positive = ["mexico"]))
print(w2v_model.wv.most_similar(positive = ["china"]))
print(w2v_model.wv.most_similar(positive = ["mexico", "trade"], negative = ["wall"]))

#%% ------------------------------------------------------------------------------------
# EX05: Explore the possibilities of the model by e.g. switching from skip-gram to cbow, using concatenation instead of averaging, 
# chosing a larger embedding size, more negative examples, etc.
print('---------- EX-05 ----------')

configs = {
    'baseline': {},
    'skipgram': {'sg': 1},
    'sum_not_mean': {'cbow_mean': 1},
    'embedding_300': {'vector_size': 300},
    'negatives_10': {'negative': 10}
}

models = {}
for name, params in configs.items():
    m = Word2Vec(workers=4, **params)
    m.build_vocab(corpus_iterable=tweets, update=False)
    m.train(corpus_iterable=tweets, total_examples=m.corpus_count, epochs=50)
    models[name] = m

for name, m in models.items():
    print(name, m.wv.most_similar('democrats', topn=3))

#%% ------------------------------------------------------------------------------------
print('#'*50)
print('########## Bigrams ##########')
print('#'*50)
#%% ------------------------------------------------------------------------------------
# EX06: Use gensim.models.phrases in order to form bigrams
# (use min_count=20, threshold=10)
print('---------- EX-06 ----------')

from gensim.models.phrases import Phrases, Phraser

phrases = Phrases(tweets, min_count=20, threshold=10)
bigram = Phraser(phrases)

#%% ------------------------------------------------------------------------------------
# EX07: Display the found bigrams (sorted alphabetically)
print('---------- EX-07 ----------')

sorted_bigrams = sorted(list(bigram.phrasegrams.items()))
print(sorted_bigrams)

#%% ------------------------------------------------------------------------------------
# EX08: Apply the phrases to the tweets
print('---------- EX-08----------')

bigram_tweets = list(bigram[tweets])
print(bigram_tweets[0])

#%% ------------------------------------------------------------------------------------
# EX09: Retrain your model based on the new corpus containing bigrams
print('---------- EX-09 ----------')

bi_model = Word2Vec(workers=(cpus - 6))
bi_model.build_vocab(corpus_iterable=bigram_tweets, update=False)
bi_model.train(corpus_iterable=bigram_tweets, total_examples=bi_model.corpus_count, epochs=100)


#%% ------------------------------------------------------------------------------------
# EX10: Select one of the bigrams and compute the cosine similarity with the sum of the 
# corresponding vectors from the unigram mode (e.g. "united" and "states" compared to "united_states")
print('---------- EX-10 ----------')

import math

# sum up the vectors of the unigrams
unigrams = w2v_model.wv['united'] + w2v_model.wv['states']

# extract the vector for the bigram
bigram = bi_model.wv['united_states']

# calculate the cosine similarity
cos_sim = sum(unigrams * bigram) / (math.sqrt(sum(unigrams ** 2)) * math.sqrt(sum(bigram ** 2)))
print(cos_sim)

#%% ------------------------------------------------------------------------------------
# EX11: Explore the embeddings for the bigrams, e.g.
print('---------- EX-11 ----------')

print(bi_model.wv.most_similar(positive = ["united_states"]))
print(bi_model.wv.most_similar(positive = ["mueller_report"]))
print(bi_model.wv.most_similar(positive = ["north_carolina"]))

#%% ------------------------------------------------------------------------------------
# EX12: Optional task: Run the Phraser again, but this time on the corpus which already contains the bigrams.
# This allows the model to build meaningful trigrams, like e.g. "new_york_times"
print('---------- EX-12 ----------')

phrases_tri = Phrases(bigram_tweets, min_count=20, threshold=10)
trigram = Phraser(phrases_tri)
trigram_tweets = list(trigram[bigram_tweets])

#%% ------------------------------------------------------------------------------------
# BLOCK 3: Learning document vector representations
# -------------------------------------------------
print('#'*50)
print('########## Doc2Vec ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Prepare the data set by transforming every tweet to a TaggedDocument
print('---------- EX-01 ----------')

tagged_tweets = [TaggedDocument(words=d, tags=['d_' + str(i)]) for i, d in enumerate(tweets)]
print(tagged_tweets[0])

#%% ------------------------------------------------------------------------------------
# EX02: Additional Task: Think about assigning multiple tags to each of the tweets. 
# This could be interesting, if we had tweets from different politicians and wanted 
# to learn additional representations for their style of tweeting.
# Try to assign a document identifier as well as the label donald_trump to all our tweets
print('---------- EX-02 ----------')

two_tagged_tweets = [TaggedDocument(words=d, tags=["d_" + str(i), 'donald_trump'])
    for i, d in enumerate(tweets)]
print(two_tagged_tweets[0])

#%% ------------------------------------------------------------------------------------
# EX03: Set up the model parameters for the Distributed memory model, build the vocab and train it
# (Now again with the corpus which documents are only assigned one tag)
print('---------- EX-03 ----------')

d2v_model = Doc2Vec(dm=1, workers=(cpus - 6))
d2v_model.build_vocab(corpus_iterable=tagged_tweets, update=False)
d2v_model.train(corpus_iterable=tagged_tweets, total_examples=d2v_model.corpus_count, epochs=100)

#%% ------------------------------------------------------------------------------------
# EX04: Chose a document and display it as a text
print('---------- EX-04 ----------')

selected_tweet = tagged_tweets[10]
pprint(' '.join(selected_tweet.words))

#%% ------------------------------------------------------------------------------------
# EX05: Find the IDs of the three most similar tweets to the one you chose
print('---------- EX-05 ----------')

ids_similar = d2v_model.dv.most_similar(selected_tweet.tags, topn=3)
pprint(ids_similar)

#%% ------------------------------------------------------------------------------------
# EX06: Display them as strings
print('---------- EX-06 ----------')

ids = [t[0] for t in ids_similar]
strings_similar = [' '.join(tweet.words)
                   for tweet in tagged_tweets
                   if tweet.tags[0] in ids]

pprint(strings_similar)

#%% ------------------------------------------------------------------------------------
# EX07: Compute the cosine similarity to the most similar one of the three tweets
print('---------- EX-07 ----------')

sim = d2v_model.dv.similarity(selected_tweet.tags[0], ids[0])
print(sim)

#%% ------------------------------------------------------------------------------------
# EX08: Train a Distributed Bag-of-words model
print('---------- EX-08 ----------')

dbow_model = Doc2Vec(dm=0, workers=(cpus - 6))
dbow_model.build_vocab(corpus_iterable=tagged_tweets, update=False)
dbow_model.train(corpus_iterable=tagged_tweets, total_examples=dbow_model.corpus_count, epochs=100)

#%% ------------------------------------------------------------------------------------
# EX09: Compare how well the two models were able to learn meaningful word embeddings
# (i.e. extract to most similar words to one pivotal word, e.g. "democrats")
print('---------- EX-09 ----------')

d2v_sim = d2v_model.wv.most_similar(positive=['democrats'])
pprint(d2v_sim)

dbow_sim = dbow_model.wv.most_similar(positive=['democrats'])
pprint(dbow_sim)

#%% ------------------------------------------------------------------------------------
# EX10: Now train a second Distributed Bag-of-words model and set the dbow_words-option to 1
# then check the most similar words to your chosen pivotal word again
print('---------- EX-10 ----------')

dbow2_model = Doc2Vec(dm=0, dbow_words=1 ,workers=(cpus - 6))
dbow2_model.build_vocab(corpus_iterable=tagged_tweets, update=False)
dbow2_model.train(corpus_iterable=tagged_tweets, total_examples=dbow2_model.corpus_count, epochs=100)

dbow2_sim = dbow2_model.wv.most_similar(positive=['democrats'])
pprint(dbow2_sim)

#%% ------------------------------------------------------------------------------------
# BLOCK 4: Learning subword vector representations
# ------------------------------------------------
print('#'*50)
print('########## FastText ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Fast Text implementation
print('---------- EX-01 ----------')

ft_model = FastText(workers=(cpus - 6))
ft_model.build_vocab(corpus_iterable=tweets, update=False)
ft_model.train(corpus_iterable=tweets, total_examples=ft_model.corpus_count, epochs=100)

#%% ------------------------------------------------------------------------------------
# EX02: Check, whether the word "example" does occur in your model's vocabulary
print('---------- EX-02 ----------')

in_vocab = 'example' in ft_model.wv.key_to_index
print(in_vocab)

#%% ------------------------------------------------------------------------------------
# EX03: Try to query your word2vec model for a vector representation of this word
print('---------- EX-03 ----------')

try:
    vec_example = w2v_model.wv["example"]
    print(vec_example)
except KeyError:
    print("Word2Vec kennt 'example' nicht — kein Vektor für OOV-Wörter.")

#%% ------------------------------------------------------------------------------------
# EX04: Now try to query your fastText model for a vector representation of this word
print('---------- EX-04 ----------')

vec_example = ft_model.wv['example']
print(vec_example)

#%% ------------------------------------------------------------------------------------
# EX05: Print the words with the most similar vector representations
print('---------- EX-05 ----------')

words_similar = ft_model.wv.most_similar(positive=['example'])
pprint(words_similar)

#%% ------------------------------------------------------------------------------------
# EX06: Check, whether the word "democrats" does occur in your model's vocabulary
print('---------- EX-06 ----------')

dem_in_vocab = 'democrats' in ft_model.wv.key_to_index
print(dem_in_vocab)

#%% ------------------------------------------------------------------------------------
# EX07: Query your word2vec and you fasttext model for a vector representation of this word
print('---------- EX-07 ----------')

w2v_dems = w2v_model.wv['democrats']
print(w2v_dems)

ft_dems = ft_model.wv['democrats']
print(ft_dems)

#%% ------------------------------------------------------------------------------------
# EX08: Print the most word with the most similar vector representations (for the w2v model)
print('---------- EX-08 ----------')

w2v_sim = w2v_model.wv.most_similar(positive=['democrats'])
pprint(w2v_sim)

#%% ------------------------------------------------------------------------------------
# EX09: Print the most word with the most similar vector representations (for the fasttext model)
print('---------- EX-09 ----------')

ft_sim = ft_model.wv.most_similar(positive=['democrats'])
pprint(ft_sim)

#%% ------------------------------------------------------------------------------------
# EX10: Do you recognize any systematic differences?
# Explore the possibilities the model by e.g. switching from skip-gram to cbow, trying different 
# n-gram ranges, chosing a larger embedding size, etc.
print('---------- EX-10 ----------')
