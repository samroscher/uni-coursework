'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 6: Neural Networks
'''

# pip uninstall h5py
# pip install h5py
# pip install keras
# pip install tensorflow
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import accuracy_score, confusion_matrix

from keras.models import Sequential
from keras.layers import Dense
# from keras.wrappers.scikit_learn import KerasClassifier
from scikeras.wrappers import KerasClassifier
from keras.models import model_from_json

import joblib
import matplotlib.pyplot as plt
import pathlib
pathlib.Path('models').mkdir(parents=True, exist_ok=True) 
pathlib.Path('plots').mkdir(parents=True, exist_ok=True) 
###############################################################################

#%% ------------------------------------------------------------------------------------
# BLOCK 1: Data Preparation 
# -------------------------
print('#'*50)
print('########## Data Preparation ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Create the 'red' dataset from the following URL:
# 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
print('---------- EX-01 ----------')

red_url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
red = pd.read_csv(red_url, delimiter=';', header=0)

#%% ------------------------------------------------------------------------------------
# Execute the following commands to set a global seed, namely the same seed as in auxiliary_functions.py to be able to compare the results later.
global_seed = 1418
np.random.seed(global_seed)

#%% ------------------------------------------------------------------------------------
# EX02: Split the dataset into
# - a pandas series 'y' with the binary target variable quality>= 6 or <6 and
# - a pandas data frame 'X' with all other variables
print('---------- EX-02 ----------')

X = red.drop('quality', axis=1)
y = (red['quality'] >= 6).astype(int)

#%% ------------------------------------------------------------------------------------
# EX03: Perform a train-test split with the ratio of 80:20
print('---------- EX-03 ----------')

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Single Layer Perceptron (SLP)
# --------------------------------------
print('#'*50)
print('########## Single Layer Perceptron (SLP) ##########')
print('#'*50)

# We are now going to build a SLP to predict the wine quality.

#%% ------------------------------------------------------------------------------------
# EX01: initialize a neural network model with keras.models.Sequential()
print('---------- EX-01 ----------')

slp = Sequential()

#%% ------------------------------------------------------------------------------------
# EX02: Add a dense layer with 11 input nodes and 1 output nodes to the SLP. 
# Set the activation function of the layer to be 'sigmoid'.
print('---------- EX-02 ----------')

slp.add(Dense(units=1, activation='sigmoid', input_dim=11))

#%% ------------------------------------------------------------------------------------
# EX03: Display an overview of the SLP
print('---------- EX-03 ----------')

print(slp.summary())

#%% ------------------------------------------------------------------------------------
# EX04: Compile the SLP and set following details: 
# 1) loss => 'binary_crossentropy'
# 2) optimizer => 'Adam'
# 3) metric => 'Accuracy'
print('---------- EX-04 ----------')

slp.compile(optimizer='Adam', loss='binary_crossentropy', metrics=['accuracy'])

#%% ------------------------------------------------------------------------------------
# EX05: # Fit the SLP on the training data.
# You can try different number for epochs and different batch sizes. Try to improve your accuracy by adjusting epochs and batch sizes. 
# Note: You have to reload the model each time so that the weights are re-initialized.
# Store the training progress in the history variable.
print('---------- EX-05 ----------')

epoch=100
batch=32
history = slp.fit(
    X_train,
    y_train,
    batch_size=batch,
    epochs=epoch,
    validation_data=(X_test, y_test)
    )

#%% ------------------------------------------------------------------------------------
# EX06: Run the following code to fit a logit model on the same training data.
print('---------- EX-06 ----------')

logit_reg = LogisticRegression()
lr = logit_reg.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX07: Use both the SLP and the Logit to predict the test data.
print('---------- EX-07 ----------')

lr_testpred = lr.predict(X_test)
perc_testpred = (slp.predict(X_test) > 0.5).astype("int32")

#%% ------------------------------------------------------------------------------------
# EX08: Compare the accuracy and confusion matrix of the two models via the following code. 
# Which model is better?
print('---------- EX-08 ----------')

print('Accuracy Perceptron:', accuracy_score(y_test, perc_testpred))
print('Accuracy Logit:', accuracy_score(y_test, lr_testpred))
print('Confusion Matrix Perceptron :')
print(confusion_matrix(y_test, perc_testpred))
print('Confusion Matrix Logit :')
print(confusion_matrix(y_test, lr_testpred))


#%% ------------------------------------------------------------------------------------
# BLOCK 2: Neural Network
# -----------------------
print('#'*50)
print('########## Neural Network ##########')
print('#'*50)


#%% ------------------------------------------------------------------------------------
# EX01: Initialize a neural network with keras.models.Sequential()
print('---------- EX-01 ----------')

nn = Sequential()

#%% ------------------------------------------------------------------------------------
# EX02: Add nine dense layers to the network with number of nodes: 12, 14, 16, 18, 16, 14, 12, 8, and 1.
# Let the activation function of all layers be 'relu', except for the last one which is 'sigmoid'.
# Note: the number of covariates must be consistant with the 'input_dim' in the first layer.
print('---------- EX-02 ----------')

nn.add(Dense(units=12, activation='relu', input_dim=11))
nn.add(Dense(units=14, activation='relu'))
nn.add(Dense(units=16, activation='relu'))
nn.add(Dense(units=18, activation='relu'))
nn.add(Dense(units=16, activation='relu'))
nn.add(Dense(units=14, activation='relu'))
nn.add(Dense(units=12, activation='relu'))
nn.add(Dense(units=8, activation='relu'))
nn.add(Dense(units=1, activation='sigmoid'))

#%% ------------------------------------------------------------------------------------
# EX03: Display an overview of the model
print('---------- EX-03 ----------')

print(nn.summary())

#%% ------------------------------------------------------------------------------------
# EX04: Compile the model and set following details: 
# 1) loss => 'binary_crossentropy'
# 2) optimizer => 'Adam'
# 3) metric => 'Accuracy'
print('---------- EX-04 ----------')

nn.compile(optimizer='Adam', loss='binary_crossentropy', metrics=['accuracy'])


#%% ------------------------------------------------------------------------------------
# EX05: # Fit the model on the training data.
# You can try different number for epochs and different batch sizes. Try to improve your accuracy by adjusting epochs and batch sizes. 
# Note: You have to reload the model each time so that the weights are re-initialized.
# Store the training progress in the history variable.
print('---------- EX-05 ----------')

history = nn.fit(X_train,
                  y_train,
                  batch_size=32,
                  epochs=500,
                  validation_data=(X_test, y_test)
                )


#%% ------------------------------------------------------------------------------------
# EX06: Keep an eye on the following plots to avoid overfitting.
# To see overfitting - increase the number of epochs.
print('---------- EX-06 ----------')

f0 = plt.figure()
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.title('Model accuracy')
plt.ylabel('Accuracy')
plt.xlabel('Epoch')
plt.legend(['Train', 'Val'], loc='upper left')
plt.show()
f0.savefig('plots/nn_training_acc.png')

f1 = plt.figure()
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Current Loss')
plt.ylabel('Loss')
plt.xlabel('Epoch')
plt.legend(['Train', 'Val'], loc='upper left')
plt.show()
f1.savefig('plots/nn_training_loss.png')

#%% ------------------------------------------------------------------------------------
# EX07: Use both the model to predict the test data.
print('---------- EX-07 ----------')

perc_testpred = (slp.predict(X_test, batch_size=32) > 0.5).astype(int)
nn_testpred = (nn.predict(X_test, batch_size=32) > 0.5).astype(int)

#%% ------------------------------------------------------------------------------------
# EX08: Compare your new model with the results of the SLP and the Logit model 
# with the following code. Which model is better?
print('---------- EX-08 ----------')

print('Accuracy Perceptron:', accuracy_score(y_test, perc_testpred))
print('Accuracy Logit:', accuracy_score(y_test, lr_testpred))
print('Accuracy NN:', accuracy_score(y_test, nn_testpred))

print('Confusion Matrix Perceptron :')
print(confusion_matrix(y_test, perc_testpred))
print('Confusion Matrix Logit :')
print(confusion_matrix(y_test, lr_testpred))
print('Confusion Matrix NN :')
print(confusion_matrix(y_test, nn_testpred))


#%% ------------------------------------------------------------------------------------
# EX09: Save the trained 'nn' model as json & h5 files.
print('---------- EX-09 ----------')
model_json = nn.to_json()
with open('models/nn_architecture.json', 'w') as json_file:
    json_file.write(model_json)

nn.save_weights('models/nn.weights.h5')

#%% ------------------------------------------------------------------------------------
# EX10: Load the model from the json and h5 files.
print('---------- EX-10 ----------')

with open ('models/nn_architecture.json', 'r') as json_file:
    loaded_model_json = json_file.read()

loaded_model = model_from_json(loaded_model_json)
loaded_model.load_weights('models/nn.weights.h5')

#%% ------------------------------------------------------------------------------------
# BLOCK 3: Grid Search
# --------------------
print('#'*50)
print('########## Grid Search ##########')
print('#'*50)

# In this exercise, a grid search is to be built to improve the model performance.

#%% ------------------------------------------------------------------------------------
# EX01: Define a function 'create_model()' that builds and returns a NN model sketched above (including the compile step).
print('---------- EX-01 ----------')
def create_model(inputs: int=11):
    nn = Sequential()

    nn.add(Dense(units=12, activation='relu', input_dim=inputs))
    nn.add(Dense(units=14, activation='relu'))
    nn.add(Dense(units=16, activation='relu'))
    nn.add(Dense(units=18, activation='relu'))
    nn.add(Dense(units=16, activation='relu'))
    nn.add(Dense(units=14, activation='relu'))
    nn.add(Dense(units=12, activation='relu'))
    nn.add(Dense(units=8, activation='relu'))
    nn.add(Dense(units=1, activation='sigmoid'))

    nn.compile(optimizer='Adam', loss='binary_crossentropy', metrics=['accuracy'])

    return nn

#%% ------------------------------------------------------------------------------------
# EX02: Use the create_model() function to create a KerasClassifier object.
print('---------- EX-02 ----------')

kc = KerasClassifier(build_fn=create_model)

#%% ------------------------------------------------------------------------------------
# EX03: create a dictionary that contains the values to be tested for
# epochs (list of integer values) and batch_size (list of integer values). 
print('---------- EX-03 ----------')

param_grid = {'epochs': [1, 10, 50],
                   'batch_size': [16, 32, 64]}

#%% ------------------------------------------------------------------------------------
# EX04: create a GridSearch CV object with 3-fold cross-validation; 
# fit it on the training data.
print('---------- EX-04 ----------')

grid = GridSearchCV(estimator=kc, param_grid=param_grid, cv=3)
grid_result = grid.fit(X_train, y_train)


#%% ------------------------------------------------------------------------------------
# EX05: Use the following codes to output the results of the grid search in the console
print('---------- EX-05 ----------')


print('Best: %f using %s' % (grid_result.best_score_, grid_result.best_params_))
means = grid_result.cv_results_['mean_test_score']
stds = grid_result.cv_results_['std_test_score']
params = grid_result.cv_results_['params']
for mean, stdev, param in zip(means, stds, params):
    print('%s (%s) with: %s' % (mean, stdev, param))


def best_saver(grid_result, X_train, y_train, X_test, y_test, h5_path='models/190904_kc.weights.h5', json_path='models/190904_kc.json'):
    best_grid=grid_result.estimator.build_fn()
    history = best_grid.fit(X_train, y_train, epochs=grid_result.best_params_['epochs'], batch_size=grid_result.best_params_['batch_size'],validation_data=(X_test, y_test)) 
    model_json = best_grid.to_json() 
    with open(json_path, 'w') as json_file: 
        json_file.write(model_json) 
    best_grid.save_weights(h5_path) 
    print('Saved the best model on disk.')
def best_loader(h5_path, json_path):
    json_file = open(json_path, 'r') 
    nn_gs_json = json_file.read() 
    json_file.close()
    nn_gs = model_from_json(nn_gs_json) 
    nn_gs.load_weights(h5_path)
    return(nn_gs)

#%% ------------------------------------------------------------------------------------
# EX06: the function best_saver() saves the best estimator found by the grid search algorithm
# The function best_loader() loads the best mode saved by best_saver().
# Using the two functions:
# 1) find the best model on the gird & save its as json and h5 files 
# 2) load the saved model 
# 3) Use the saved model to make prediction on the test data
print('---------- EX-06 ----------')


best_saver(grid_result, X_train, y_train, X_test, y_test)

nn_gs = best_loader(h5_path='models/190904_kc.weights.h5', json_path='models/190904_kc.json')

nn_gs_testpred = (nn_gs.predict(X_test) > 0.5).astype('int32')


#%% ------------------------------------------------------------------------------------
# BLOCK 4: Comparison with Previous Models
# ----------------------------------------
print('#'*50)
print('########## Comparison with Previous Models ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: # Load the results of the models from previous Exercise / Binary Classification
print('---------- EX-01 ----------')

# results_gbc = joblib.load(...)
# results_rf = results_gbc = joblib.load(...)

#%% ------------------------------------------------------------------------------------
# EX02: Use the following codes to compare the preset results with the previous results
print('---------- EX-02 ----------')

# print('Accuracy Perceptron:', accuracy_score(y_test, perc_testpred))
# print('Accuracy NN:', accuracy_score(y_test, nn_testpred))
# print('Accuracy NN_GS:', accuracy_score(y_test, nn_gs_testpred))
# print('Accuracy Logit:', accuracy_score(y_test, lr_testpred))
# print('Accuracy RF:', results_rf['test_accuracy'])
# print('Accuracy GBC:', results_gbc['test_accuracy'])

# print('Confusion Matrix Perceptron :')
# print(confusion_matrix(y_test, perc_testpred))
# print('Confusion Matrix NN :')
# print(confusion_matrix(y_test, nn_testpred))
# print('Confusion Matrix NN_GS :')
# print(confusion_matrix(y_test, nn_gs_testpred))
# print('Confusion Matrix Logit :')
# print(confusion_matrix(y_test, lr_testpred))
# print('Confusion Matrix RF :')
# print(results_rf['test_confusion'])
# print('Confusion Matrix GBC :')
# print(results_gbc['test_confusion'])
# %%
