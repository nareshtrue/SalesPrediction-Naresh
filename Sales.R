#Required Libraries
library("hier.part")
#install.packages('hier.part')
#install.packages('dplyr')
library('installr')
library('MuMIn')
#install.packages('MuMIn')
library('MuMIn')
setwd('C:\\Users\\v-gimupp\\Downloads')
data1<-read.csv('train12.csv')

#Taking Numerical and Categorical data 
library("dplyr")
Num_data<-select_if(data, is.numeric)
Catogorical_Data<-select_if(data,is.factor)

is.factor(Catogorical_Data$credit_score_range)

Catogorical_Data$credit_score_range <- as.numeric(Catogorical_Data$credit_score_range)

data<- cbind.data.frame(Num_data,Catogorical_Data)
data <- subset(data,select=-c( time_zone) )


#Splitting the data
library('caTools')
split <- sample.split(data$total_sales, SplitRatio = 0.75)
trainingData<-subset(data, split==TRUE)
testData <- subset(data, split==FALSE)
testData1<- subset(testData,select=-c(total_sales) )
testData_Y<- subset(testData,select=c(total_sales) )


model<- lm(total_sales ~.  ,data = trainingData)
summary(model)

predictedValues<-as.data.frame(predict(model,testData1))

#Fitting linear model to numerical data and selecting numerical predictors
model<- lm(total_sales ~  employee_size ,data = data)
summary(model)

log(testData_Y)

AIC(model)

#SVM
library('e1071')
svmModel<-svm(formula =  total_sales ~ .
              , data=trainingData,kernel="poly", degree=2)

Predicted_SVC=as.data.frame(predict(svmModel,testData1))

summary(svmModel)
  
  #RMSE
  
  #a<-sqrt(mean(abs(fit$residuals)))
  #a<-(mean(abs(fit1$residuals)))
  sqrt( mean( (testData_Y-testData$total_sales)^2 , na.rm = TRUE ) )
RMSE <- sqrt(mean((log(testData_Y)-predictedValues)^2))



RMSE_LM <-sqrt(mean((Error_SVM)^2))

RMSE_LM
RMSE_SVM
model$coefficients
library('car')
VIF<-as.data.frame(vif(model))

#Finding Correlation in Factor variables

chisq.test(Catogorical_Data)

str(Catogorical_Data)

cor<-cor(Num_data1,y=NULL)


numdata<-selec
str(data)
var(data$avg_age)

#Distribution of response variable
boxplot(data$total_sales)

#Dummyfing
install.packages('dummies')
library(dummies)
Numerical_Data<-dummy.data.frame(data,names = c('city','state','store_location','time_zone','location_employee_code','credit_score','credit_score_range'))

names(Numerical_Data)
var(Numerical_Data)

#To remove multicolliniearity 
PCA <- prcomp(Numerical_Data,scale. = TRUE)
PCA$sdev
PCA$rotation
PCA<-princomp(Numerical_Data,cor = TRUE)

# Getting the all possible combination of independent variables
a=colnames(testData1)
x=a
b=c()
count=0
for ( i in 1:length(x)){
  print(i)
  if(grepl(x[i],a[i+1],fixed = TRUE)== FALSE){
    d<-  paste(x[i],a[i+1], sep='+')
    a=c(a,d)
  }
  for (j in 1:(length(a))){
    
    if (x[i] != a[length(x)]){
      if((grepl(x[i],a[j+2],fixed = TRUE) == FALSE) ){
        d<-  paste(x[i],a[j+2], sep='+')
        a=c(a,d)
      }
    }
  } 
}
#print(a)



#Fitting the models for each and every possible comnination of features
fit <- c()
for (i in 1:lenght(a)){
#Multiple LineaRegression
fit <-lm(formula = total_sales ~ a[i] , data = trainingData)
}

#Finding AIC values for all fitted models
A_Information_Criterian <- c()
for (i in 1:length(fit)){
  A_Information_Criterian <- AIC(fit[i])
}

#Finding VIF values for each and every 
Variable_Inflation_Factor <- c()
library('car')
for (i in 1:length(fit)){
  Variable_Inflation_Factor <- vif(fit1)
}

#GGPLOT for each and every independent variable with response variable
#fit<-c()
models<-data.frame()
b<-as.list(a)
for(i in 1:length(a)){
  #print(typeof(b[i]))
  models<-(lm(trainingData$total_sales ~ unlist(trainingData[a[i]]),data=as.data.frame(cbind(unlist(trainingData[a[i]]),trainingData$total_sales))))
  #models<-b[i]
  #y_pred[i] = predict(fit[i], newdata = test_set)
}

models(fit[1])
fit[18]
  
  fit1 <- lm(trainingData$total_sales ~ unlist(trainingData['total_household_size']), data=trainingData)
  
 #typeof(fit1['coefficients'])
as.in

typeof(trainingData$city)
typeof(trainingData['city'])



library(ggplot2)

ggplot() +
  geom_point(aes(x = trainingData$employee_size, y = trainingData$total_sales),
             colour = 'red') +
  geom_line(aes(x = trainingData$employee_size, y = predict(model, newdata = trainingData)),
            colour = 'blue') +
  ggtitle('Sales vs Emp_Size (Training set)') +
  xlab('emp size') +
  ylab('sales')


#SVM
library('e1071')
svmModel<-svm(formula =  total_sales ~ avg_age+	blue_collar	+white_collar+	total_household_size +	total_household_income
, data=trainingData,kernel="linear")

AIC(fit,fit1,fit2,fit3,fit4,fit5,fit6)
BIC(fit,fit1,fit2,fit3,fit4,fit5,fit6)

options(na.action = "na.fail")
a=dredge(fit)
#Predicting on test data
y1<-as.data.frame(predict(svmModel,testData1))
y<-as.data.frame((predict(fit1,testData1)))
actaulValues<-as.data.frame(testData$total_sales)
compare<-cbind.data.frame(actaulValues,y1)
                                        #y<-predict(fit1,testData1['employee_size'])
Error_SVM<- y1 - testData$total_sales
Error_LM<- y - testData$total_sales


#Automated model Selection
options(na.action = "na.fail")
fit <-lm(formula = total_sales ~  avg_age	+ total_household_size +	total_household_income + location_employee_code +	employee_size +	credit_score	 , data = data)
y=dredge(fit)
x <- min(y$AICc)

#SVM
install.packages('e1071')
library('e1071')
fit1<-svm(formula = total_sales ~ . , data = data, kernel='linear')
summary(fit1)

sum(fit1$residuals)

y <- plot(fit1,data = data)
y




