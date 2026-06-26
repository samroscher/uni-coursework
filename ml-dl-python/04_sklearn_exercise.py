'''
Course: Machine Learning and Deep Learning with Python
SoSe 2026
LMU Munich, Department of Statistics
Exercise 4: Sklearn - continuous response variable
'''

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
import joblib
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import mean_squared_error
import pathlib
pathlib.Path('models').mkdir(parents=True, exist_ok=True)
pathlib.Path('plots').mkdir(parents=True, exist_ok=True)

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

#%% ------------------------------------------------------------------------------------
# EX01: Set a global seed
print('---------- EX-01 ----------')
np.random.seed(26062026)

#%% ------------------------------------------------------------------------------------
# EX02: Split the dataset into
# - a pandas series 'y' with the target variable quality, and
# - a pandas data frame 'X' with all other variables
print('---------- EX-02 ----------')

X = red.drop('quality', axis=1)
y = red['quality']
print(X)
print(y)

#%% ------------------------------------------------------------------------------------
# EX03: Perform a train-test split with the ratio of 80:20
print('---------- EX-03 ----------')
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)
# Hint: X_train, X_test, y_train, y_test = ...

#%% ------------------------------------------------------------------------------------
# BLOCK 2: Random Forest
# ----------------------
print('#'*50)
print('########## Random Forest ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Define a Random Forest Regressor with 100 trees
print('---------- EX-01 ----------')
rf = RandomForestRegressor(n_estimators=100)

#%% ------------------------------------------------------------------------------------
# EX02: Fit the model on the training data
print('---------- EX-02 ----------')
rf.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX03: Make predictions on the test data and print the MSE
print('---------- EX-03 ----------')
predictions_rf = rf.predict(X_test)
mse_rf = mean_squared_error(y_test, predictions_rf)

#%% ------------------------------------------------------------------------------------
# EX04: Define a dictionary that contains a set of possible values for the max_features and max_depth. 
# Such a dictionary can be used for hypertuning via cross-validation
print('---------- EX-04 ----------')
hyperparameters_rf = {'max_features': [None, 'sqrt', 'log2'],
                      'max_depth': [None, 5, 3, 1]}

#%% ------------------------------------------------------------------------------------
# EX05: Create a GridSearch CV object with 10-fold cross-validation and fit it on the training data
print('---------- EX-05 ----------')
gs_rf = GridSearchCV(rf, hyperparameters_rf, cv=10)
gs_rf.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX06: Print the best hyperparameters combination 
print('---------- EX-06 ----------')
params_rf = gs_rf.best_params_
print(params_rf)


#%% ------------------------------------------------------------------------------------
# EX07: Let the best RandomForest model make predictions on the test data and print the MSE
print('---------- EX-07 ----------')
predictions_rf_tuned = gs_rf.predict(X_test)
mse_rf_tuned = mean_squared_error(y_test, predictions_rf_tuned)

#%% ------------------------------------------------------------------------------------
# BLOCK 3: Gradient Boosting
# --------------------------
print('#'*50)
print('########## Gradient Boosting ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Define a Gradient Boosting Regressor with 100 trees
print('---------- EX-01 ----------')
gbr = GradientBoostingRegressor(n_estimators=100)

#%% ------------------------------------------------------------------------------------
# EX02: Fit the model on the training data
print('---------- EX-02 ----------')
gbr.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX03: Make predictions on the test data and print the MSE
print('---------- EX-03 ----------')
predictions_gbr = gbr.predict(X_test)
mse_gbr = mean_squared_error(y_test, predictions_gbr)

#%% ------------------------------------------------------------------------------------
# EX04: Define a dictionary that contains a set of possible values for the max_features and max_depth. 
# Such a dictionary can be used for hypertuning via cross-validation
print('---------- EX-04 ----------')
hyperparameters_gbr = {'max_features': [None, 'sqrt', 'log2'],
                       'max_depth': [None, 5, 3, 1]}

#%% ------------------------------------------------------------------------------------
# EX05: Create a GridSearch CV object with 10-fold cross-validation and fit it on the training data
print('---------- EX-05 ----------')
gs_gbr = GridSearchCV(gbr, hyperparameters_gbr, cv=10)
gs_gbr.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX06: Make predictions on the test data and print the MSE
print('---------- EX-06 ----------')
predictions_gbr_tuned = gs_gbr.predict(X_test)
mse_gbr_tuned = mean_squared_error(y_test, predictions_gbr_tuned)

#%% ------------------------------------------------------------------------------------
# BLOCK 4: Linear Model
# ---------------------
print('#'*50)
print('########## Linear Model ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Define a linear regression model with sklearn.linear_model.LinearRegression and fit it on the training data
print('---------- EX-01 ----------')
lm = LinearRegression()
lm.fit(X_train, y_train)

#%% ------------------------------------------------------------------------------------
# EX02: Make predictions on the test data and print the MSE
print('---------- EX-02 ----------')
predictions_lm = lm.predict(X_test)
mse_lm = mean_squared_error(y_test, predictions_lm)

#%% ------------------------------------------------------------------------------------
# BLOCK 5: Model Comparison
# -------------------------
print('#'*50)
print('########## Model Comparison ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Compare Random Forest, GBR, and Linear Model.
# Additionally, check the performance of a naive model (i.e., a model that always returns the mean of y.) 
print('---------- EX-01 ----------')
mse_naive = mean_squared_error(y_test, np.full(len(y_test), y_train.mean()))

mse_table = pd.DataFrame({
    'model': ['lm', 'rf', 'gbr', 'naive'],
    'mse': [mse_lm, mse_rf_tuned, mse_gbr_tuned, mse_naive]
})

print(mse_table)

#%% ------------------------------------------------------------------------------------
# EX02: Which model performs better in terms of the MSE?
print('---------- EX-02 ----------')

answer  = 'rf performed best, marginally better than gbr. ' \
'But both performed distinctly better than lm and naive.'
print(answer)


#%% ------------------------------------------------------------------------------------
# EX03: Print the feature importance of the best model
print('---------- EX-03 ----------')
pd.DataFrame(gs_rf.best_estimator_.feature_importances_,
             index = X.columns,
             columns=['value']).sort_values(by='value', ascending=False)

#%% ------------------------------------------------------------------------------------
# BLOCK 6: Model Evaluation
# -------------------------
print('#'*50)
print('########## Model Evaluation ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Adjust the following sample code to create a scatter plot of the predicted vs. the true values for the best model.
# You may need to firstly create a subfolder 'plots' in your working directory
print('---------- EX-01 ----------')

import matplotlib.pyplot as plt
f0 = plt.figure()
plt.scatter(y_test, predictions_rf_tuned)
plt.title('Scatter plot prediction vs. truth')
plt.ylabel('Prediction')
plt.xlabel('y')
plt.show(block=False)
plt.pause(3)
plt.close()
f0.savefig('plots/scatter.png')


#%% ------------------------------------------------------------------------------------
# EX02: Customize the following sample code to sketch a histogram and a kernel density estimation of the prediction errors
print('---------- EX-02 ----------')

import seaborn as sns
f1 = plt.figure()
plt.title('Histogramm pred error best model')
sns.histplot(y_test - predictions_rf_tuned, kde=True, bins=25)
plt.xlabel('Pred Error')
f1.savefig('plots/hist_kde.png')

f2 = plt.figure()
plt.title('Histogramm pred error mean model')
sns.histplot(y_test - y_train.mean(), kde=False, bins=25)
plt.xlabel('Pred Error')
f2.savefig('plots/hist_kde2.png')

#%% ------------------------------------------------------------------------------------
# BLOCK 7: Saving/Loading Model
# -----------------------------
print('#'*50)
print('########## Saving/Loading Model ##########')
print('#'*50)

#%% ------------------------------------------------------------------------------------
# EX01: Save the trained model with joblib.dump() as a .pkl file for later use
print('---------- EX-01 ----------')
joblib.dump(gs_rf.best_estimator_, './models/rf_tuned.pkl')

#%% ------------------------------------------------------------------------------------
# EX02: How do you load the saved model?
print('---------- EX-02 ----------')
rf_model = joblib.load('./models/rf_tuned.pkl')
