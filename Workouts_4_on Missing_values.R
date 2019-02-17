library('Hmisc')
install.packages('Hmisc')

setwd('C:\\Users\\v-gimupp\\Downloads\\TechGig Data sets')

data<- read.csv('train12.csv')


nrow(data[!complete.cases(data),])

manyNAs(data = data, 0.1)

sum(is.null(data$location_employee_code))

sum(is.nan(data$location_employee_code[4,]))

imputeddata<-impute(data$female, mean)

imputedata<- ifelse(is.na(data$female), median(data$female, na.rm = TRUE), data$female)

data$female[is.na(data$female)]<- mean(data$female, na.rm = TRUE)

trimws(' Gireeshku  ')
colnames(data[6])

data2<- data.frame(rep(1,304))
for (i in 1:ncol(data)){
  #print(colnames(data[i]))
  if (is.numeric(unlist(data[i])) == TRUE){
    
    data[i][is.na(data[i])] <- mean(unlist(data[i]), na.rm= TRUE)
    #print(sum(is.na(data[i])))
    
    #print(colnames(data[i]))
    data2<-cbind.data.frame(data2, data[i])
   # data2[i]<-data[i]
    else
      
      data[i][is.na(data[i])] <- mode(data[i], na.rm= TRUE)
      #data2<- cbind.data.frame(data2, data[i])
      #data$colnames(data[i])[is.na(data$colnames(data[i])] <- mode(data$colnames(data[i]), na.rm= TRUE)
    
  
  }
}

data1<- knni
is.integer()

is.factor(data['total_sales'])
str(data)


a<-factor(A,A,G,A)
mode(a)
median(a)
median()
x='female'
is.na(((data['female'])))

unlist()
install.packages('VIM')
library(VIM)

summary(data)
data_cat <- kNN(data=data,variable = c('location_employee_code'), k=5)

?kNN

library('DMwR')

data_Cat<-knnImputation(data, k=3, scale = T, meth='median')

knn

data$location_employee_code <- as.factor(data$location_employee_code)

class(data$location_employee_code)

typeof(data$location_employee_code)
sum(is.na(data$location_employee_code))
data$location_employee_code <-factor(data$location_employee_code, levels = c(colnam))