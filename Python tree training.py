# Databricks notebook source
# region and farm type into numeric
# explore features
# reducing size of df

# COMMAND ----------

# Data Processing
import pandas as pd
import numpy as np

# Modelling
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, confusion_matrix, precision_score, recall_score, ConfusionMatrixDisplay
from sklearn.model_selection import RandomizedSearchCV, train_test_split
from sklearn.preprocessing import OneHotEncoder
from scipy.stats import randint
from sklearn import neighbors

knn = neighbors.KNeighborsClassifier(n_neighbors=5)
# Tree Visualisation
from sklearn.tree import export_graphviz
from IPython.display import Image
#import graphviz



# COMMAND ----------

wfm_sfi = pd.read_csv("/dbfs/mnt/lab/unrestricted/elm/uptake/wfm_sfi.csv") 

# COMMAND ----------

print(wfm_sfi.columns)

# COMMAND ----------

display(wfm_sfi)

# COMMAND ----------

OneHotEncoder(wfm_sfi[['arable_intro','arable_inter','grassland_intro','grassland_inter','moorland']])

#OneHotEncoder(wfm_sfi[['arable_intro','arable_inter','grassland_intro','grassland_inter','moorland']])

# COMMAND ----------

wfm_sfi['agreement_numeric'] = pd.factorize(wfm_sfi["agreement"])[0]

# COMMAND ----------

# Split the data into features (X) and target (y)
X = wfm_sfi[['participation_schemes',
       'ha_parcel_uaa', 'lu_livestock',
       'ha_total_eligible_sfi_arable_soils_inter',
       'payment_total_sfi_arable_soils_inter', 'profit_sfi_arable_soils_inter',
       'profit_perha_sfi_arable_soils_inter',
       'ha_total_eligible_sfi_arable_soils_intro',
       'payment_total_sfi_arable_soils_intro', 'profit_sfi_arable_soils_intro',
       'profit_perha_sfi_arable_soils_intro',
       'ha_total_eligible_sfi_improved_grassland_inter',
       'payment_total_sfi_improved_grassland_inter',
       'profit_sfi_improved_grassland_inter',
       'profit_perha_sfi_improved_grassland_inter',
       'ha_total_eligible_sfi_improved_grassland_intro',
       'payment_total_sfi_improved_grassland_intro',
       'profit_sfi_improved_grassland_intro',
       'profit_perha_sfi_improved_grassland_intro',
       'ha_total_eligible_sfi_moorland_intro',
       'payment_total_sfi_moorland_intro', 'profit_sfi_moorland_intro',
       'profit_perha_sfi_moorland_intro', 'aonb',
       'gm_parcel', 'gm_per_ha']]
y = wfm_sfi['agreement_numeric']

# Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

# COMMAND ----------

rf = RandomForestClassifier()
rf.fit(X_train, y_train)

# COMMAND ----------

y_pred = rf.predict(X_test)
#save model function


# COMMAND ----------

accuracy = accuracy_score(y_test, y_pred)
print("Accuracy:", accuracy)

# COMMAND ----------



# COMMAND ----------



# COMMAND ----------



# COMMAND ----------

param_dist = {'n_estimators': randint(50,500),
              'max_depth': randint(1,20)}

# Create a random forest classifier
rf = RandomForestClassifier()

# Use random search to find the best hyperparameters
rand_search = RandomizedSearchCV(rf, 
                                 param_distributions = param_dist, 
                                 n_iter=5, 
                                 cv=5)

# Fit the random search object to the data


# COMMAND ----------

rand_search.fit(X_train, y_train)

# COMMAND ----------

# Create a variable for the best model
best_rf = rand_search.best_estimator_

# Print the best hyperparameters
print('Best hyperparameters:',  rand_search.best_params_)

# COMMAND ----------

# Generate predictions with the best model
y_pred = best_rf.predict(X_test)

# Create the confusion matrix
cm = confusion_matrix(y_test, y_pred)

ConfusionMatrixDisplay(confusion_matrix=cm).plot();

# COMMAND ----------

# create a more balanced data set 1:2
# add in the features - one hot encoding 
# 


# COMMAND ----------

y_pred = knn.predict(X_test)

accuracy = accuracy_score(y_test, y_pred)
precision = precision_score(y_test, y_pred)
recall = recall_score(y_test, y_pred)

print("Accuracy:", accuracy)
print("Precision:", precision)
print("Recall:", recall)

# COMMAND ----------


