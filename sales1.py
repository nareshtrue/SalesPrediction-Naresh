import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import mean_squared_error
from math import sqrt

#importing the dataset
traindata=pd.read_csv("C:\\Users\\v-gimupp\\Documents\\train.csv")

X=traindata.iloc[:,[6,17]].values.astype(float)
y=traindata.iloc[:,21].values.astype(float)

#Splitting test and train data
from sklearn.cross_validation import train_test_split
x_train, x_test, y_train, y_test = train_test_split(X, y, test_size = 0.20, random_state = 1)

#Creating object and fitting the model
from sklearn.linear_model import LinearRegression
l=LinearRegression()
plreg=PolynomialFeatures(degree=2)
plreg1=plreg.fit_transform(x_train)
model=l.fit(plreg1, y_train)
RefCorr=traindata.corr()
#y_train.shape

# Predicting the test results

TestPred=model.predict(plreg.fit_transform(x_test))

def rootMeanSquaredError(actual_Y,expected_Y):    
    rmse = sqrt(mean_squared_error(actual_Y,expected_Y))
    return rmse

rootMeanSquaredError(TestPred,y_test)


   





