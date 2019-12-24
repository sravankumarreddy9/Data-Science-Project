# -*- coding: utf-8 -*-
"""
Created on Wed Dec 18 19:00:42 2019

@author: reddymv
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
motor = pd.read_csv("pmsm_temperature_data.csv")
motor.shape
motor.head()
motor.isnull().sum()
#1
print(motor['ambient'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['ambient'], color='g', bins=100, hist_kws={'alpha': 0.4});
#2
print(motor['coolant'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['coolant'], color='g', bins=100, hist_kws={'alpha': 0.4});
#3
print(motor['u_d'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['u_d'], color='g', bins=100, hist_kws={'alpha': 0.4});
#4
print(motor['u_q'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['u_q'], color='g', bins=100, hist_kws={'alpha': 0.4});
#5
print(motor['i_d'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['i_d'], color='g', bins=100, hist_kws={'alpha': 0.4});
#6
print(motor['i_q'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['i_q'], color='g', bins=100, hist_kws={'alpha': 0.4});
#7
print(motor['torque'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['torque'], color='g', bins=100, hist_kws={'alpha': 0.4});
#8
print(motor['motor_speed'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['motor_speed'], color='g', bins=100, hist_kws={'alpha': 0.4});
#9
print(motor['pm'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['pm'], color='g', bins=100, hist_kws={'alpha': 0.4});
#10
print(motor['stator_yoke'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['stator_yoke'], color='g', bins=100, hist_kws={'alpha': 0.4});
#11
print(motor['stator_tooth'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['stator_tooth'], color='g', bins=100, hist_kws={'alpha': 0.4});
#12
print(motor['stator_winding'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['stator_winding'], color='g', bins=100, hist_kws={'alpha': 0.4});
#13
print(motor['profile_id'].describe())
plt.figure(figsize=(9, 8))
sns.distplot(motor['profile_id'], color='g', bins=100, hist_kws={'alpha': 0.4});

motor.hist(figsize=(16, 20), bins=50, xlabelsize=8, ylabelsize=8);

#correlation
motor_corr = motor.corr()

cor_target = abs(motor_corr["pm"])#Selecting highly correlated features
relevant_features = cor_target[cor_target>0.5]
relevant_features

cor_target_sy = abs(motor_corr["stator_yoke"])#Selecting highly correlated features
relevant_features_sy = cor_target[cor_target>0.5]
relevant_features_sy
'''
motor_corr = motor.corr()['pm','stator_yoke'][:, -1] # -1 because the latest row is SalePrice
golden_features_list = motor_corr[abs(motor_corr) > 0.5].sort_values(ascending=False)
print("There is {} strongly correlated values with SalePrice:\n{}".format(len(golden_features_list), golden_features_list))
'''
#optional check
columns = np.full((motor_corr.shape[0],), True, dtype=bool)
for i in range(motor_corr.shape[0]):
    for j in range(i+1, motor_corr.shape[0]):
        if motor_corr.iloc[i,j] <= 0.5:
            if columns[j]:
                columns[j] = False
selected_columns = motor.columns[columns]
motor_corr_sel = motor[selected_columns]

#standardize data
from sklearn.preprocessing import scale
motor_standardize = pd.DataFrame(scale(motor))
motor_standardize.columns = motor.columns

#treating outliers
sns.boxplot(data=motor_standardize)
q1 = motor_standardize.quantile(0.25)
q3 = motor_standardize.quantile(0.75)
IQR = q3 - q1
from scipy import stats
z = np.abs(stats.zscore(motor_standardize))
print(z)
threshold = 3
print(np.where(z > 3))
print(IQR)
print((motor_standardize < (q1 - 1.5 * IQR)) |(motor_standardize > (q3 + 1.5 * IQR)))
motor_standardize = motor_standardize[(z < 3).all(axis=1)]
#removing outliers
motor_standardize = motor_standardize[~((motor_standardize < (q1 - 1.5 * IQR)) |(motor_standardize > (q3 + 1.5 * IQR))).any(axis=1)]
motor_standardize.shape



#alternatives
outliers = pd.DataFrame(motor_standardize[(motor_standardize < (q1 -1.5 * IQR)) | (motor_standardize > (q3 + 1.5 * IQR))])
outliers.dropna(how ='all', inplace =True)
outliers.count()

outliers_extreme = pd.DataFrame(motor_standardize[(motor_standardize < (q1 -3 * IQR)) | (motor_standardize > (q3 + 3 * IQR))])
outliers_extreme.dropna(how ='all', inplace =True)
outliers_extreme.count()
torque_outliers = pd.DataFrame(outliers_extreme['torque'])
ambient_outliers = pd.DataFrame(outliers_extreme['ambient'])
iq_outliers = pd.DataFrame(outliers_extreme['i_q'])

torque_outliers.dropna(inplace=True)
ambient_outliers.dropna(inplace=True)
iq_outliers.dropna(inplace=True)

#splitting the data

Y= motor_standardize[['pm','stator_yoke']]
X=motor_standardize.drop(['pm', 'stator_yoke'], axis=1)
from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.25)

#linear regression
from sklearn.linear_model import LinearRegression
lr = LinearRegression()
model = lr.fit(X_train, Y_train)
y_pred = model.predict(X_test)

from sklearn import metrics

from sklearn.metrics import mean_squared_error, r2_score

print('Mean Absolute Error:', metrics.mean_absolute_error(Y_test, y_pred))
print('Mean Squared Error:', metrics.mean_squared_error(Y_test, y_pred))
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(Y_test, y_pred)))
rsqu = r2_score(Y_test, y_pred)
r2_score(Y_test, y_pred)
print('R2 value is:', rsqu)

#Random forests


from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import confusion_matrix
from sklearn.tree import export_graphviz
from sklearn.externals.six import StringIO 
from IPython.display import Image 
from pydot import graph_from_dot_data
from sklearn import tree
from sklearn.preprocessing import StandardScaler
from sklearn import preprocessing
import seaborn as sns
from sklearn.preprocessing import MinMaxScaler
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import cross_val_predict
from sklearn.ensemble import RandomForestRegressor

from sklearn.ensemble import RandomForestRegressor

regressor = RandomForestRegressor(n_estimators=20, random_state=0)
regressor.fit(X_train, Y_train)
y_pred_random = regressor.predict(X_test)

print('Mean Absolute Error:', metrics.mean_absolute_error(Y_test, y_pred_random))
print('Mean Squared Error:', metrics.mean_squared_error(Y_test, y_pred_random))
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(Y_test, y_pred_random)))
rsqu = r2_score(Y_test, y_pred_random)
r2_score(Y_test, y_pred_random) #R2 = 0.9995330610814697
print('R2 value is:', rsqu)

X_grid = np.arange(min(X_train), max(X_train), 0.01)  
  
# reshape for reshaping the data into a len(X_grid)*1 array,  
# i.e. to make a column out of the X_grid value                   
X_grid = X_grid.reshape((len(X_grid), 1)) 
  
# Scatter plot for original data 
plt.scatter(X_train, Y_train, color = 'blue')   
  
# plot predicted data 
plt.plot(X_grid, regressor.predict(X_grid),  
         color = 'green')  
plt.title('Random Forest Regression') 
plt.xlabel('Position level') 
plt.ylabel('Salary') 
plt.show()

#Decision tree regression 
regressor_dt = DecisionTreeRegressor()
regressor_dt.fit(X_train, Y_train)
y_pred_dt = regressor_dt.predict(X_test)


print('Mean Absolute Error:', metrics.mean_absolute_error(Y_test, y_pred_dt))
print('Mean Squared Error:', metrics.mean_squared_error(Y_test, y_pred_dt))
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(Y_test, y_pred_dt)))
print('R2 value for decision trees"', r2_score(Y_test, y_pred_dt)) #R2 = 0.9990533025182136

from sklearn.model_selection import cross_val_predict

fig, ax = plt.subplots()
ax.scatter(Y_test, y_pred_dt, edgecolors=(0, 0, 0))
ax.plot([Y_test.min(), Y_test.max()], [Y_test.min(), Y_test.max()], 'k--', lw=4)
ax.set_xlabel('Actual')
ax.set_ylabel('Predicted')
ax.set_title("Ground Truth vs Predicted")
plt.show()

from sklearn import tree
from sklearn.externals.six import StringIO
from IPython.display import Image
from sklearn.tree import export_graphviz
import pydotplus
import graphviz 
#dot_data = tree.export_graphviz(model_dt, out_file='tree.dot') 
dot_data = StringIO()
export_graphviz(regressor_dt, out_file=dot_data, max_depth=5, filled=True, rounded=True, special_characters=True)
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
Image(graph.create_png())
tree.plot_tree(regressor_dt, max_depth=None, feature_names=None, class_names=None, label='all', filled=False, impurity=True, node_ids=False, proportion=False, rotate=False, rounded=False, precision=3, ax=None, fontsize=None)


#xgboost

from xgboost import plot_importance
from matplotlib import pyplot
from sklearn.model_selection import cross_val_score,KFold
from sklearn.metrics import mean_absolute_error
import matplotlib.pyplot as plt
from scipy.stats import skew
from collections import OrderedDict
import xgboost as xgb
from sklearn.model_selection import GridSearchCV
from xgboost.sklearn import XGBRegressor
from sklearn.multioutput import MultiOutputRegressor
xgb_multi =MultiOutputRegressor(XGBRegressor()).fit(X_train, Y_train)
model_xgb=xgb.XGBClassifier(n_estimators=100, learning_rate=0.08, gamma=0, subsample=0.75,
                           colsample_bytree=1, max_depth=7)
xgb_train_rmse = np.mean((xgb_multi.predict(X_train) - Y_train) ** 2, axis = 0)
xgb_test_rmse = np.mean((xgb_multi.predict(X_test) - Y_test) ** 2, axis = 0)
xgb_train_rmse = np.sqrt(xgb_train_rmse)
xgb_test_rmse = np.sqrt(xgb_test_rmse)

xgb_train_pred = xgb_multi.predict(X_train)
xgb_train_r2 = r2_score(Y_train, xgb_train_pred)
xgb_test_pred = xgb_multi.predict(X_test)
xgb_test_r2 = r2_score(Y_test, xgb_test_pred)

xgb_train_score = xgb_multi.score(X_train, Y_train)
xgb_test_score = xgb_multi.score(X_test, Y_test)

#polynomial regression 

from sklearn.linear_model import LinearRegression
lr = LinearRegression()
import operator
from sklearn.preprocessing import PolynomialFeatures
polynomial_features= PolynomialFeatures(degree=2)
xtrain_poly = polynomial_features.fit_transform(X_train)
lr.fit(xtrain_poly, Y_train)
y_poly_pred = lr.predict(xtrain_poly)
rmse_poly = np.sqrt(mean_squared_error(Y_train,y_poly_pred))
r2_poly = r2_score(Y_train,y_poly_pred)
print(rmse_poly)
print(r2_poly)








