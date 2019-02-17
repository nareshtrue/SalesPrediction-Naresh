setwd('your file path')

data<- read.csv('UniversalBank.csv')

#Checking the null Values
colSums(is.na(data))
#Checking the empty Values
colSums(data=='')

#Imputing missing values
library('mice')
md.pattern(ActualData)
imputed_Data <- mice(ActualData, m=5, maxit = 50, method = 'pmm', seed = 500) 
data_Selected<- complete(imputed_Data,2)

#Checking the number NA's now
sum(is.na('data_selected'))

#Removing the identities
Install.packages(dplyr)
data_Selected <- data_Selected %>% select(-c(ID, ZIP.Code))

#Structure of the data
str(data_Selected)

# converting Experience, Income and Mortgae into numerics
data_Selected$Experience<- as.numeric(data_Selected$Experience)
data_Selected$Income <- as.numerica(data_Selected$Income)
data_Selected$Mortgage <- as.numeric (data_Selected$Mortgage)

#Checking the number of unique values to identify factors
sapply(data_Selected, function(x) length(unique(x)))
#Converting categoricals into factors
data_Selected$Personal.Loan -> as.factor(data_Selected$Personal.Loan)
data_Selected$Family -> as.factor(data_Selected$Family)
data_Selected$Securities.Account -> is.factor(data_Selected$Securities.Account)
data_Selected$CD.Account -> as.factor(data_Selected$CD.Account)
data_Selected$Online -> as.factor(data_Selected$Online)
data_Selected$CreditCard -> as.factor(data_Selected$CreditCard)

#Summary of the dataset
summary(data_Selected)



#Splitting the data into train and test

library(caTools)
set.seed(1)
sample<- sample.split(data_Selected$Personal.Loan, SplitRatio = .75)
Training_set <- subset(data_Selected, sample==T)
Test_set<- subset(data_Selected, sample==F)

#Building the model
library('e1071')
model<- svm (formula= Personal.Loan~.,data = Training_set,
               type='C-classification',
               kernel='radial')
summary(model)

#Predicting the test results
y_pred<-predict(model, Test_set[,8])

#Confusion matrix
library(caret)
confusionMatrix(table(y_pred, Test_set[,10]))


#Hyper parameter Tuning
tuneResult <- tune(svm, train.x = data_Selected[-8], train.y = data_Selected[,8], ranges = list(gamma = 10^(-6:-1), cost = 2^(2:3)))
print(tuneResult)
tunedModel <- tuneResult$best.model
y_pred <- predict (tunedModel, Test_set[-8], type='response')
y_pred<- ifelse(y_pred>0.5, 1, 0)
confusionMatrix(y_pred, Test_set[-8])

#ROC Curve
install.packages(ROCR)
library (ROCR)
predictions <- predict(model, newdata=Test_Data[-10], type='response')
ROCRpred <- prediction(as.numeric(predictions), Test_Data$Personal.Loan)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

