import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.svm import SVR
import statsmodels.formula.api as sm
from math import sqrt

#importing the dataset
traindata=pd.read_csv("C:\\Users\\v-gimupp\\Documents\\train.csv")
testdata=pd.read_csv("C:\\Users\\v-gimupp\\Documents\\test.csv")
y=traindata.iloc[:,21].values
z=traindata.iloc[:, :-1] 

#A function for preprocessing
def preprocessing(X):
#traindata.describe().transpose()
    
    
    ##Taking only Categoricals and coverting them into dummies
    City=pd.get_dummies(X.city)
    store_location=pd.get_dummies(X.store_location)
    state=pd.get_dummies(X.state)
    time_zone=pd.get_dummies(X.time_zone)
    location_employee_code=pd.get_dummies(X.location_employee_code)
    credit_score=pd.get_dummies(X.credit_score)
    credit_score_range=pd.get_dummies(X.credit_score_range)
    #CategoricalX_=pd.get_dummies([])

    #Making categories into array
    CategoricalX=pd.concat([City,store_location,state,time_zone,location_employee_code,credit_score,credit_score_range],axis=1)


    ##Taking only numericals
    NumericalX=X.iloc[:,[6,7,8,9,10,11,12,17,20]]

    RefinedData=pd.concat([NumericalX,CategoricalX],axis=1)
    return RefinedData

refdata=preprocessing(z)


#Defining RMSE
def rootMeanSquaredError(actual_Y,expected_Y):    
    rmse = sqrt(mean_squared_error(actual_Y,expected_Y))
    return rmse



#For automated backward selection
def backwardElimination(x, sl):
    
    numVars = len(x[0])
    print (numVars)
   ## print(y)
    for i in range(0, numVars):
        
        regressor_OLS = sm.OLS(y, x).fit()
        maxVar = max(regressor_OLS.pvalues).astype(float)
        if maxVar > sl:
            
            for j in range(0, numVars-(i+1)):
                
                if (regressor_OLS.pvalues[j].astype(float) == maxVar):
                    
                    x = np.delete(x, j, 1)
                    print(i)
    regressor_OLS.summary()
    return x
  
    
SL = 0.05
X_opt = refdata.iloc[:, ].values

X_Modeled = backwardElimination(X_opt, SL)

#Splitting test and train data for evaluation purpose
from sklearn.cross_validation import train_test_split
x_train, x_test, y_train, y_test = train_test_split(refdata, y, test_size = 0.20, random_state = 1)



#Creating object and fitting training Data
lr=LinearRegression()
plreg=PolynomialFeatures(degree=2)
PolyRefinedData=plreg.fit_transform(x_train)
model1=lr.fit(PolyRefinedData,y_train)

#model2=lr.fit(PolyRefinedData,y_train)

#Evaluating score

RefinedPred=model1.predict(plreg.fit_transform(x_test))
rootMeanSquaredError(y_test,RefinedPred)


# To test using SVR
svr=SVR(kernel='linear', C=0.5)
model=svr.fit(x_train,y_train)

RefinedPred=model.predict(x_test)
rootMeanSquaredError(y_test,RefinedPred)




#Now lets import TestData and transform it
testdata1=preprocessing(testdata)
RefinedPred1=model1.predict(plreg.fit_transform(testdata1))


def outliers_z_score(ys):
    threshold = 3

    mean_y = np.mean(ys)
    stdev_y = np.std(ys)
    z_scores = [(y - mean_y) / stdev_y for y in ys]
    return np.where(np.abs(z_scores) > threshold)

a=[]

for i in range(0,len(X_Modeled.T)):
    p=outliers_z_score(X_Modeled[i])
    a.append(p)
    
    # Removing outliers using Standard Diviation
    from scipy import stats
  a=  stats.describe(X_Modeled)
 stdScalar=StandardScaler()
 final_list = [i for i in X_Modeled if (i > mean - 2 * sd)] 
 final_list = [i for i in final_list if (i < mean + 2 * sd)] 
 print(final_list) 
















    

    
    
    




##Importing onehotencoder for converting categoricals into numericals/binary

from sklearn.preprocessing import OneHotEncoder
ohe=OneHotEncoder()
Ohe_Categorical=ohe.fit_transform(CategoricalX)


#from sklearn.preprocessing import StandardScaler
#x_std=StandardScaler()
#standardisedData=x_std.fit_transform(X)

import statsmodels.formula.api as SM

b = np.correlate(NumericalX,NumericalX)
print (b)


plt.scatter(traindata.iloc[:, 12].values.astype(int), y, color = 'red')
#plt.plot(X, regressor.predict(X), color = 'blue')
plt.title('Truth or Bluff (Regression Model)')
plt.xlabel('Position level')
plt.ylabel('Salary')
plt.show()


