
setwd('C:\\Users\\v-gimupp\\Downloads\\Azure Machine learning\\Day - 4 - Unsupervised\\20170810_CSE9098c_UnuspervisedTechniques_Day04\\Lab material')
data<-read.csv('CustomerData.csv')

num_data <- data[,-c(1,2,3,4,5,11,12,13,14)] 

x<-cor(num_data)
b<-eigen(cov(num_data))
b$values
eigen(x)

y<-princomp(num_data, cor = TRUE)

z<-prcomp(num_data, cor = TRUE)
summary(y)^2
biplot(y)

names(y)
mean(num_data$Tenure)
sd(num_data$Tenure)

y$sdev
y$loadings
y$center # mean of input features/columns
y$scale # Standard Diviation of input features and columns
y$n.obs # no of observations of imput data set
class(y$scores)
X<-as.matrix(y$loadings)*as.matrix(y$scores)




a<-scale(num_data, center = FALSE)
y$call

z$scale 
Z$rotation = class(as.matrix(y$loadings))
z$x
sum(z$rotation[,1]^2)





# Sum of squares of loadings for each principal companet is 1, 
#since principal componets are linear combinations of loadings and predictors
sum(y$loadings[,1]^2) 
str(num_data)
summary(y)

a<-y$sdev^2
b<-a/sum(a)

g<-z$sdev^2
h<-g/sum(g)

a
b
g
h
eigen(cor(num_data))$vectors
eigen(cov(num_data))$vectors 

y$loadings
y$scores
corrplot::corrplot(x,method='square',type = 'full')