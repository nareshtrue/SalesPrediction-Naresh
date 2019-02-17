setwd('C:\\Users\\v-gimupp\\Downloads\\TechGig Data sets')
data1<-read.csv('train.csv')
UnseenData<-read.csv('test.csv')

coplot(total_sales ~ employee_size| , data = as.data.frame(Num_data))


str(UnseenData)
UnseenData$employee_size<- log(UnseenData$employee_size)

sum(is.na(data1))




#Omitting nulls and Na's
data2<-na.omit(data1)

#Taking Numerical and Categorical data 
library("dplyr")
Num_data<-select_if(data2, is.numeric)
Catogorical_Data<-select_if(data2,is.factor)

UnseenNum_data<-select_if(UnseenData, is.numeric)
UnseenCatogorical_Data<-select_if(UnseenData,is.factor)


#Dummyfing Categorical
#install.packages('dummies')
library(dummies)
Catogorical_Data<-dummy.data.frame(Catogorical_Data)
data3 <- cbind.data.frame(Num_data,Catogorical_Data)


UnseenCatogorical_Data<-dummy.data.frame(UnseenCatogorical_Data)
UnseendataTransformedData <- cbind.data.frame(UnseenNum_data,UnseenCatogorical_Data)


#plotting the density of distribution without log transformation
plot(density(log(data3$employee_size)) )#for Total sales also as they have high skewness

#plotting the density of distribution with log transformation
plot(density(log(data3$employee_size))) # total sales also

#data4<-subset(data3, total_sales < 340000)

#Log Transformations to avoid negetive values in predictions 

dataWitLog<-subset(data3, total_sales < 230000)
#summary(data)
dataWitLog$employee_size<- log(dataWitLog$employee_size)
dataWitLog$total_sales<- log(dataWitLog$total_sales)

#and removed outliers
plot(density(data$total_sales))

library('car')



#Splitting the data into test and train
library('caTools')
set.seed(6)
split <- sample.split(dataWitLog$total_sales, SplitRatio = 0.75)
trainingData<-subset(dataWitLog, split==TRUE)
testData <- subset(dataWitLog, split==FALSE)
testData1<- subset(testData,select=-c(total_sales) )
testData_Y<- exp(subset(testData,select=c(total_sales) ))


#selected the variables using this single factor analysis
correlation<-cor(Num_data)
unique(data1$state)
#avg_age+female employee_size+avg_age+location_employee_code
#avg_age+employee_size+avg_age+location_employee_codeB+location_employee_codeC+location_employee_codeD+location_employee_codeE
#+location_employee_codeH+location_employee_codeI
#fitting LM
model<- lm(total_sales ~ avg_age+female+employee_size+avg_age+
             location_employee_codeA+ location_employee_codeB + location_employee_codeC +location_employee_codeD +location_employee_codeE + location_employee_codeF+location_employee_codeG+location_employee_codeH+location_employee_codeI 
           ,data = dataWitLog)

model<-lm(total_sales ~ employee_size+
          location_employee_codeA+ location_employee_codeB +
            location_employee_codeC +location_employee_codeD +
            location_employee_codeE + location_employee_codeF+
            location_employee_codeG+location_employee_codeH +avg_age,
            
          data = dataWitLog) 
summary(model)

AIC(model)


#Finding Multicollinearity in Numerical attributes do not use dummified categorical variables to detect best possible combination and best model

library('car')
vif(model)


#for selecting optimal model
AIC(model)
chi
#Evaluating results with test data
Predicted_Y<-as.data.frame(ceiling(exp(predict(model,testData1))))
RMSE <- sqrt(mean((testData_Y-Predicted_Y)^2))
error <- as.integer(unlist(abs(testData$total_sales-Predicted_Y)))
MSE <- mean(error)
errorMedian<-median(error)

Unseen_Total_sales <-as.data.frame(ceiling(exp(predict(model,UnseendataTransformedData))))
TestData_TotalSales <- cbind.data.frame(UnseenData$outlet_no, Unseen_Total_sales)

write.csv(TestData_TotalSales, file = "TestDataPrediction10.csv")

writ
##Plotting for employee size,below works only when model is training with employee_size
library(ggplot2)

ggplot() +
  geom_point(aes(x = dataWitLog$employee_size  , y = dataWitLog$total_sales),
             colour = 'red') +
  geom_line(aes(x = dataWitLog$employee_size  , y = predict(model, newdata =dataWitLog )),
            colour = 'blue') +
  ggtitle('Sales vs Emp_Size (Training set)') +
  xlab('emp size') +
  ylab('sales')

print("Writing complete")


#Removing outliers

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


