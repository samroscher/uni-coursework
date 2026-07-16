"""Exercise 2 - Representing Documents"""

import argparse
import os
from urllib import request

from bs4 import BeautifulSoup
import pandas as pd

# Import following modules from sklearn:
# - LabelBinarizer
# - CountVectorizer, TfidfVectorizer
# - train_test_split
# - accuracy_score, confusion_matrix
# - LogisticRegression
from sklearn.preprocessing import LabelBinarizer
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.linear_model import LogisticRegression


def main(file_path: str) -> None:
    """Perform sentiment analysis on a document."""

    print('#' * 50)
    print('### 01: PREPARE DOCUMENT')
    print('#' * 50)

    print('---------- EX-01 ----------')

    # Load the csv file as a pandas dataframe
    imdb = pd.read_csv(file_path)
    print(imdb.shape)

    print('---------- EX-02 ----------')

    # Extract the texts and labels from the dataframe
    reviews_raw = imdb['review']
    sentiments = imdb['sentiment']

    # We BeautifulSoup for removing the html markup
    reviews_clean = [BeautifulSoup(rev, 'html.parser').get_text() for rev in reviews_raw]

    print('---------- EX-03 ----------')

    # Transform the labels into a binary array.
    # Hint: You can use the `LabelBinarizer` for this task
    lb = LabelBinarizer()
    labels = lb.fit_transform(sentiments.to_numpy())
    print(labels.shape)

    print('---------- EX-04 ----------')

    # Perform a split into a training set and a test set with proportion 80 to 20
    # Hint: You can use the `train_test_split` function

    y_train, y_test, x_train, x_test = train_test_split(
        labels, reviews_clean, test_size=0.2, random_state=160726
    )

    print("Number of training examples:", len(x_train))
    print("Number of training labels:", len(y_train))
    print("Number of test examples:", len(x_test))
    print("Number of test labels:", len(y_test))

    print('#' * 50)
    print('### 02: BAG-OF-WORDS')
    print('#' * 50)

    # Define a `CountVectorizer`
    # Use:
    # - Unigrams
    # - A minimum term frequency of 10
    # - Counts (not binary indicators)
    # - A maximum of 10k features

    cv = CountVectorizer(min_df=10, max_features=10000, binary=False)

    # Feed the training set to the vectorizer and create a Document-Term Matrix
    bow_train = cv.fit_transform(x_train)

    # Transform your test set to a DTM as well
    bow_test = cv.transform(x_test)

    # Check the dimensions of your data
    print(f'BOW Train dim: {bow_train.shape}, BOW Test dim: {bow_test.shape}')

    print('#' * 50)
    print('### 03: LINEAR REGRESSION')
    print('#' * 50)

    print('---------- EX-01 ----------')

    # Sklearn doesn't like vectors
    y_train = y_train.ravel()
    y_test = y_test.ravel()

    # Define a logistic regression model
    # Feel free to investigate and play with different parameters
    model = LogisticRegression()

    # Train the model
    model = model.fit(bow_train, y_train)

    print('---------- EX-02 ----------')
    # Predict the sentiments of the test set
    preds_test = model.predict(bow_test)
    print('Test predictions:', preds_test[:5])

    # Check the accuracy of the predictions
    accuracy = accuracy_score(y_test, preds_test)
    print('Accuracy:', accuracy)

    # Inspect the confusion matrix
    conf_mat = confusion_matrix(y_test, preds_test)
    print(conf_mat)

    print('#' * 50)
    print('### 04: FINDING A BETTER FIT')
    print('#' * 50)

    # Not bad, right? But we can do better.
    # Note down the accuracy and try the following options:

    # 1. Try ngrams (uni-, bi-, tri-grams)
    # 2. Increase max_features to 50k
    # 3. Back to the basic setting, but use tf-idf
    # 4. Now use tf-idf + ngrams (uni-, bi-, tri-grams)
    # 5. Set the max_features option up to 50k again
    # 6. Optional: Think of other parameters to tweak in order to increase performance

    def run_experiment(name, vectorizer, x_train, x_test, y_train, y_test):
        bow_train = vectorizer.fit_transform(x_train)
        bow_test = vectorizer.transform(x_test)
        model = LogisticRegression(max_iter=1000)
        model.fit(bow_train, y_train)
        preds = model.predict(bow_test)
        acc = accuracy_score(y_test, preds)
        print(f'{name}: {acc:.4f}')
        return acc

    configs = {
    '1_count_ngram':        CountVectorizer(ngram_range=(1,3), min_df=10, max_features=10000),
    '2_count_ngram_50k':    CountVectorizer(ngram_range=(1,3), min_df=10, max_features=50000),
    '3_tfidf_basic':        TfidfVectorizer(ngram_range=(1,1), min_df=10, max_features=10000),
    '4_tfidf_ngram':        TfidfVectorizer(ngram_range=(1,3), min_df=10, max_features=10000),
    '5_tfidf_ngram_50k':    TfidfVectorizer(ngram_range=(1,3), min_df=10, max_features=50000),
    }

    results = {}
    
    for name, vec in configs.items():
        results[name] = run_experiment(name, vec, x_train, x_test, y_train, y_test)

    # 1_count_ngram: 0.8772
    # 2_count_ngram_50k: 0.9046
    # 3_tfidf_basic: 0.8991
    # 4_tfidf_ngram: 0.9043
    # 5_tfidf_ngram_50k: 0.9097

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