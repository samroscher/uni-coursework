"""Exercise 2 - Representing Documents"""

import argparse
import os
from urllib import request

from bs4 import BeautifulSoup
import pandas as pd

# Import a Count- and TfidfVectorizer from sklearn
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer


def main(file_path: str) -> None:
    """Vectorize a document."""

    print('#' * 50)
    print('### 01: PREPARE DOCUMENT')
    print('#' * 50)

    print('---------- EX-01 ----------')

    # Load the csv file as a pandas dataframe
    imdb = pd.read_csv(file_path)

    # Inspect the first few rows to see what's inside
    print(imdb.head())

    print('---------- EX-02 ----------')

    # Extract the texts from the dataframe
    reviews_raw = imdb['review']

    # We BeautifulSoup for removing the html markup
    reviews_clean = [BeautifulSoup(rev, 'html.parser').get_text() for rev in reviews_raw]

    # Have a look some of the cleaned reviews
    print(reviews_clean[:5])

    print('#' * 50)
    print('### 02: BAG-OF-WORDS')
    print('#' * 50)

    print('---------- EX-01 ----------')

    # Define a CountVectorizer as 'unigram_vectorizer'
    # Use:
    # - Unigrams
    # - A minimum term frequency of 10
    # - Counts (not binary indicators)
    # - A maximum of 10k features

    unigram_vectorizer = CountVectorizer(min_df=10, max_features=10000)

    # If we restrict the maximum number of features,
    # according to which criterion are the features omitted?

    # Feed the data to the vectorizer and create a Document-Term Matrix (DTM)
    bow = unigram_vectorizer.fit_transform(reviews_clean)

    # Which dimension does the DTM have?
    dims = bow.shape
    print(dims)

    print('---------- EX-02 ----------')

    # Set the minimum term frequency to 100,
    # create a new DTM and check what happens to the dimension
    unigram_vectorizer.min_df = 100
    print(unigram_vectorizer.fit_transform(reviews_clean).shape)

    # Create a DTM using uni-, bi- & trigrams
    # Don't restrict the maximum number of features in order
    # to see what happens to the dimension. Leave the minimum term frequency at 100.
    # You might run into memory issues otherwise.
    # You will also encounter a massive impact on the runtime

    print('---------- EX-03 ----------')

    multigram_vectorizer = CountVectorizer(ngram_range=(1,3), min_df=100)
    multigram_dtm = multigram_vectorizer.fit_transform(reviews_clean)

    print('---------- EX-04 ----------')

    # Create a DTM using uni- & bigrams while simultaneously applying TF-IDF weighting
    tfidf_vectorizer = TfidfVectorizer(ngram_range=(1,2), min_df=10, max_features=10000)
    tfidf_dtm = tfidf_vectorizer.fit_transform(reviews_clean)

    # Inspect the entries of the first 5 rows & the first 5 columns
    subset = tfidf_dtm[:5, :5].toarray()

    print(subset)


def download_data(target: str) -> None:
    """Download the IMBD dataset as a csv to the target location."""

    if os.path.exists(target):
        print('File already exists. Skipping download.')
        return

    target_root = os.path.split(target)[0]
    os.makedirs(target_root, exist_ok=True)

    url = 'https://raw.githubusercontent.com/assenmacher-mat/' \
          'nlp_notebooks/master/imdb.csv'
    request.urlretrieve(url, target)


if __name__ == '__main__':
    # Set CLI option for file path
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--file', default='data/imdb.csv', type=str)
    args = parser.parse_args()

    # Download necessary files
    download_data(args.file)

    # Execute main program
    main(args.file)