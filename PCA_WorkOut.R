setwd('C:\\Users\\v-gimupp\\Downloads\\TechGig Data sets')
data<- read.csv('train.csv')

#Imputing Zero Values in the variables

data$blue_collar<-ifelse(data$blue_collar == 0, median(data$blue_collar),
                         data$blue_collar)


data$white_collar<-ifelse(data$white_collar == 0, median(data$white_collar),
                          data$white_collar)

sum(data$blue_collar == 0)


data$white_collar<-ifelse(data$total_household_size == 0, median(data$total_household_size),
                          
                          
                          data$total_household_size)

#Taking numerical and Categorical

library('dplyr')
Numerical_Data<-select_if(data,is.numeric)
Categorical_Data<-select_if(data,is.factor)

#Imputing NA's and zeros
sum(is.na(data))

sum(is.null(data))

is.nan(is.null(data))

# Dumifying categorical values
library('dummies')
Cat_Num_Data <- dummy.data.frame(Categorical_Data)
Total_Data<-cbind.data.frame(Numerical_Data,Cat_Num_Data)



# Removing identifiers and response Variables
Required_Data <- Total_Data[, -c(1,3,15)]

# Splitting into train and test
library('caTools')
set.seed(1)
sample <- sample.split(Required_Data$total_sales, SplitRatio = .98)
Traindata<-subset(Required_Data,sample==TRUE)
Testdata<-subset(Required_Data,sample==FALSE)


#Training PrinComp Object with the test data to get PC's
prin_comp <- prcomp(Required_Data, cor = TRUE)

Required_Data[,15]
names(prin_comp)

#Cenetr is the mean to variables
prin_comp$center

# Loadings of PC's withrespect to 
a<-prin_comp$rotation

#Computing variance using Standard Deviation 
Var <- prin_comp$sdev^2

#Finding the proportion Variance to know how the contribution of variance of each variable
proportion_varience <- Var/sum(Var)

#Plotting which will help us to find the required no. of PC's
plot(proportion_varience, xlab = 'Principal_Coponents' ,ylab = 'Varience_Proportion', type = 'b')

#Plotting commulative sum of Propotion of variance
plot(cumsum(proportion_varience), xlab = 'Principal_Coponents' ,ylab = 'Cummulative_Varience_Proportion', type = 'b')

'Taking the required No. of PC\'s as we can conclude using above graph'
Principal_components <- prin_comp$x[,1:168]

#Predicting PC's on Test data using Train PC's object
Test_Princomp<-predict(prin_comp, Testdata)

#Taking only required PC's
OptimizedTestPCs<-Test_Princomp[,1:168]

"Note:- We should not find PC's on the whole data (train and test data) at a time"
""
"PCA can be applied only on Numerical variable, if any Catagorical, need to dummify"