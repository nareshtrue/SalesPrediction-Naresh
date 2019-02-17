setwd('D:\\All\\Azure Machine learning\\Day - 4 - Unsupervised\\20170810_CSE9098c_UnuspervisedTechniques_Day04\\Lab material')

install.packages('xlsx')
library('xlsx')
data<-read.xlsx('polutiondata.xlsx', 1)
data<-read.csv('customerData.csv')
str(data)
typeof(data)
summary(data)
library('dplyr')

numdata<-select_if(data,is.integer)
NumData<- select_if(data,  is.integer)
CatData<- select_if(data, is.factor)
NumData<- numdata[,-c(1,12)]

library('dummies')

dummifiedCatData<-dummy.data.frame(CatData)

DummyData<-dummy.data.frame(CatData)
FullData<-cbind.data.frame(NumData,DummyData)
prcomp<-prcomp(FullData,scale. = TRUE)
pinncomp<-princomp(FullData,cor = TRUE)
class(pinncomp$scores)
loadings<-as.matrix(pinncomp$loadings)

Pcomponents<-as.vector(loadings)*pinncomp$scores
dim(Pcomponents)
dim(prcomp$x)
x<-(as.data.frame(FullData))
sd(x$FavoriteGameUniform)
typeof(FullData)
prcomp$x
pinncomp$scores

sqrt(eigen(var(cor(FullData))$vectors))
a<-prcomp$rotation
sum(a[2,]^2)

sc<-as.data.frame(scale(FullData))
mean(as.numeric(unlist(sc)))

a<-as.data.frame(prcomp$rotation)

a

var <-prcomp$sdev^2

pro_var<-var/sum(var)

plot(pro_var,xlab = "No of PC's",ylab = "Pro Variance", type="b")

plot(cumsum(pro_var),xlab = "No of PC's",ylab = "Pro Variance", type="b")