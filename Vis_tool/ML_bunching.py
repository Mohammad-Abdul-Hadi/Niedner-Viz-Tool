# -*- coding: utf-8 -*-
"""
Created on Mon Jul 26 08:29:25 2021

@author: miladrmz
"""
# Import libraries
import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression, Ridge
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR, LinearSVR
from sklearn.metrics import r2_score

import shap

#%% Read raw data
filename = 'Bunching_Max_feb2021_Full.csv'
raw = pd.read_csv(filename, encoding='cp1252')

input_names = ['Modulus of elasticity - Inner - Warp (MPa)',
       'Modulus of elasticity - Inner - Weft (MPa)',
       'Shear modulus - Inner (MPa)',
       'Modulus of elasticity - Outer - Warp (MPa)',
       'Modulus of elasticity - Outer - Weft (MPa)',
       'Shear modulus - Outer (MPa)', 'Friction coefficient',
       'Axial Load (N/mm^2)', 'Pre-strain (%) - Inside jacket 0.44%',
       'Pre-strain (%) - Inside jacket 1.6%', 'Pre-strain (%) - Outside jacket 0.5%']

#%% Bunching | Split x and y and preprocess
raw.drop(['Unnamed: 0', 'sample'], axis = 1, inplace=True)

x_raw = raw.iloc[:,:-1].copy()
y_raw = raw.iloc[:,-1].copy()
x_num = x_raw.iloc[:,:-1].copy()
x_cat = x_raw.iloc[:,-1].copy()

# encode the cat feature
encoder = OneHotEncoder()
x_cat_encoded = encoder.fit_transform(np.array(x_cat).reshape(-1, 1)).toarray()

# concat the num and cat
x_prc = np.hstack([np.array(x_num), x_cat_encoded])

# Standardize the data
scaler = StandardScaler()
x_scaled = scaler.fit_transform(x_prc)

# split to train test
x_train, x_test, y_train, y_test = train_test_split(x_scaled, y_raw, test_size=0.25)

#%% Training
models = [LinearRegression(),
          Ridge(alpha=100),
          RandomForestRegressor(n_estimators=100),
          GradientBoostingRegressor(n_estimators=100),
          #SVR(kernel = 'rbf', gamma = 0.01, C = 10000, epsilon= 0.001),
          LinearSVR(max_iter=10000)]

for model in models:
    model.fit(x_train, y_train)
    y_pred = model.predict(x_test)
    print(r2_score(y_test, y_pred))
    print(r2_score(y_train, model.predict(x_train)))

#%% plot pareto plots
from matplotlib import pyplot as plt

model = RandomForestRegressor(n_estimators=100)
model.fit(x_train, y_train)
y_pred = model.predict(x_test)
print(r2_score(y_test, y_pred))
plt.scatter(y_test, y_pred, s = 20, c = 'blue')
plt.plot([0, max(y_test)], [0, max(y_test)], c='black')
plt.ylabel('Prediction (mm)', size = 17)
plt.xlabel('Observation (mm)', size = 17)
plt.xlim([0, max(y_test)])
plt.ylim([0, max(y_test)])
plt.savefig('Bunching_gb.png', dpi = 400)

#%% SHAP Analysis

model = RandomForestRegressor(n_estimators=100)
model.fit(x_train, y_train)

explainer = shap.Explainer(model)
X_shap = pd.DataFrame(x_train, columns = input_names)
shap_values = explainer(X_shap)

#%%
shap.plots.beeswarm(shap_values, show = False)
plt.savefig('Shap_bunching.png', dpi=400, bbox_inches="tight")
plt.savefig('Shap_bunching.svg', dpi=400, bbox_inches="tight")

plt.show()











