library(dplyr)
library(mice)
library(caTools)
library(randomForest)

#Reading the data from drive and assigning to data frame variable
data<-read.csv('D:\\Desktop-2\\Amexpert_Train\\train.csv')
head(data) #Viewing the sample data
str(data) #Structure of the data


#Checking the missing values column wise
colSums(is.na(data)) #For NA's
 
colSums(data=='') #For empty spaces

#checking the unique values of each column
sapply(data, function(x) length(unique(x)))

#Imputing NA's and empty values
data$user_depth[is.na(data$user_depth)] <-3
data$gender[data$gender=='']<-'Male'
data$age_level[data$age_level]<-2
#data$city_development_index[is.na(data$city_development_index)]<-2

#To know the percentage of missing values column wise
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data1,2,pMiss)
#apply(data1,1,pMiss)

#Removing the Identity values
#install.packages('dplyr')
library(dplyr)
data1<- data %>% select(-c(session_id,DateTime,user_id,product_category_2,city_development_index))


#checking the unique values of each column
sapply(data1, function(x) length(unique(x)))



#Library for Imputing missing values 
library(mice)
#install.packages('mice')
#methods(mice)

imputed_data<- mice(data1,m=5,method='polyreg', maxit=2 ,seed=56)
imputed_data1<-complete(imputed_data,2)
apply(imputed_data1,2,pMiss)

imputed_data1$gender[imputed_data1$gender=='""']

sum(is.nan(imputed_data1$gender))

sapply(imputed_data1, function(x) length(unique(x)))
str(imputed_data1)

library(openxlsx)
#write.csv(imputed_data1,file="D:\\American_Express_ImputedData.csv")

a<-read.csv("D:\\American_Express_ImputedData.csv")
data2<-a[,-1]


#Factorizing all the given columns
for (i in c("campaign_id","webpage_id","product_category_1","user_group_id","age_level","user_depth","var_1","is_click")){
  data2[,i]=as.factor(data2[,i])
}

#Viewing the barchart()
barchart(data2$campaign_id)

imputed_data$is_click<- as.numeric(imputed_data$is_click)

Factored_Data<- imputed_data1 %>% select(-c(is_click))

#str(imputed_data1)
#Dummifying categorical factors
#install.packages('dummies')
library(dummies)
Dummified_data <- dummy.data.frame(Factored_Data, sep="_")

#Combining dummmified features and target variable
full_Preprocessed_Data<-cbind.data.frame(Dummified_data,imputed_data1$is_click)

#Splitting the train and test
install.packages('caTools')
library(caTools)
set.seed(6)
split<-sample.split(data2$is_click, SplitRatio = 0.75)
train_data<-subset(data2,split==TRUE)
test_data<-subset(data2,split==FALSE)

table(a$is_click)
barchart(data2$webpage_id)
plot(data2$webpage_id, data2$is_click, data2$is_click=="1" )
#sample<-sample(full_Preprocessed_Data$`imputed_data1$is_click`,nrow(full_Preprocessed_Data)*0.75)
#train_data<-full_Preprocessed_Data[sample,]
#test_data<-full_Preprocessed_Data[-sample,]

library(lattice)
library(grid)
#install.packages('DMwR')
library(DMwR)
#Check the distribution of target variable if classes are balanced
table(train_data$is_click)
#Smoting the data in order to balance the dataset
smoted_train_data<-SMOTE(is_click~.,train_data,perc.over = 200)
#write.csv(smoted_train_data,file="D:\\Smoted_train_data.csv")

table(smoted_train_data$is_click)


#Logistic regression
model<-glm(is_click ~ ., family = binomial, data = smoted_train_data )
summary(model)
library(e1071)
#install.packages('ROCR')
library(ROCR)
confusionMatrix(test_data[,10],as.factor(y_prediction))
#Predictions on train data
y_pred<-predict(model,smoted_train_data[,-10])
y_prediction<-ifelse(y_pred>0.5,1,0)
table(as.factor(smoted_train_data[,10]),as.factor(y_prediction))

#Predictions on test data
y_pred<-predict(model,test_data[,-10],type = 'response')
y_prediction<-ifelse(y_pred>0.5,1,0)
table(test_data[,10],y_prediction)
library(InformationValue)
#install.packages('InformationValue')
confusionMatrix(as.factor(smoted_train_data[,10]),as.factor(y_prediction))

#Plotting ROC curve
ROCRpred <- prediction(y_prediction, test_data$is_click)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

#Calculating AUC
auc<-performance(ROCRpred, measure = 'auc')
auc <- auc@y.values[[1]]


specificity(smoted_train_data[,10],as.factor(y_prediction))
precision(smoted_train_data[,10],as.factor(y_prediction))
((2*0.7244449*0.8406247)/(0.7244449+0.8406247))

#plot(imputed_data1$webpage_id)

------------------------------------------------------------------
#Decision trees

library(rpart)
model<-rpart(is_click~., data=smoted_train_data,method="class")
plot(model)
text(model)
varImp(model)
text(model,pretty = 0)
dim(y_prediction)

#Predictions on train data
y_pred<-predict(model,smoted_train_data[,-10], type = "class")
#y_prediction<-ifelse(y_pred>0.5,1,0)
table(as.factor(smoted_train_data[,10]),as.factor(y_pred))
confusionMatrix(smoted_train_data[,10],as.factor(y_pred))


y_pred<-predict(model,test_data[,-10], type = "class")
#y_prediction<-ifelse(y_pred>0.5,1,0)
table(as.factor(test_data[,10]),as.factor(y_pred))
confusionMatrix(test_data[,10],as.factor(y_pred))

#Plotting ROC curve


ROCRpred <- ROCR::prediction(as.numeric(y_pred), as.numeric(test_data$is_click))
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

#Calculating AUC
auc<-performance(ROCRpred, measure = 'auc')
auc <- auc@y.values[[1]]
auc
----------------------------------------------------------------------------
#randomForest()
colnames(smoted_train_data)
set.seed(1234) 
rf<-randomForest(is_click~., data=smoted_train_data, 
                 ntree=350,mtree=3)
rf
varImpPlot(rf)


#Parameter tuning for RF
set.seed(123)
trf<-tuneRF(smoted_train_data[,-10],
       smoted_train_data$is_click,
       mtryStart = 3,
       ntreeTry = 50,
       stepFactor = 1,
       improve = 0.001,
       trace = T,
       plot = T,
       doBest = T,
       importance=T)

#Prediction on test data
y_pred<-predict(rf,test_data[,-10], type = "class")
#y_prediction<-ifelse(y_pred>0.5,1,0)
table(as.factor(test_data[,10]),as.factor(y_pred))
confusionMatrix(test_data[,10],as.factor(y_pred))
class((y_pred))

#Plotting ROC curve
ROCRpred <- prediction(as.numeric(y_pred), as.numeric(test_data$is_click))
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

#Calculating AUC
auc<-performance(ROCRpred, measure = 'auc')
auc <- auc@y.values[[1]]
auc

#XGBOOST
-------------------------------------------------------------------------
#XGbooast
library(xgboost)
#install.packages('xgboost')
library(Matrix)
library(magrittr)

#Creating matrix one-hot encoding for factor variables
trainm<-sparse.model.matrix(is_click ~ .-1, data = train_data)
head(trainm)
colnames(trainm)
train_label<-as.integer(train_data[,'is_click'])
train_matrix <- xgb.DMatrix(as.matrix(trainm),label=train_label)


testm<-sparse.model.matrix(is_click ~ .-1, data = test_data)
head(testm)
colnames(testm)
test_label<- as.integer(test_data[,'is_click'])
str(test_label)
test_matrix <- xgb.DMatrix(as.matrix(testm),label=test_label)

#parameter
no_class <- length(unique(train_label))
xgb_params <- list("objective"="multi:softprob",
                   "eval_mertic"="mlogloss",
                   "num_class"=no_class
                   )
watchlist<-list(train=train_matrix, test=test_matrix)
str(test_data)

best_model<-xgb.train(params = xgb_params,
                      data = train_matrix,
                      nrounds = 100,
                      watchlist = watchlist)




