#Classifications workouts

#1.K-NN

setwd('C:\\Users\\v-gimupp\\Downloads\\TechGig Data sets')
dataset<-read.csv('IRIS_Data_Set.csv')

#dataset<-read.csv('IRIS Data set With 2 Classes modified.csv')
colnames(dataset)
str(dataset)

summary(dataset)
#colnames(dataset)<-c('x1','x2','x3','x4','x5','x6','x7','x8','y')

#factorizing output variables
#dataset$y<- factor(dataset$y,labels= c(0,1,2), levels = c('setosa','versicolor','virginica'))


#To see the distribution of attributes
plot(density(dataset$Sepal_Length))

#To see the distribution of Classes/factors
hist(as.numeric(dataset$Species))

#Splitting into train and test
library('caTools')
set.seed(1)
split<- sample.split(dataset$Species, SplitRatio = 0.75)
training_set<- subset(dataset, split== FALSE)
test_set<- subset(dataset, split== TRUE)

#Feauture_Scaling

training_set[,-5]=scale(training_set[,-5])
test_set[,-5]<-scale(test_set[,-5])


#1.LogisticRegression

model<- glm(formula = Species ~., 
            family = binomial,
            data = training_set)

ypred<-predict(model, type='response', newdata=test_set[-5])
ypred<-ifelse(ypred>0.5, 1,0)

#2.KNN

library('class')
ypred<- knn(training_set[,-5],test_set[,-5], cl=training_set[,5], k=5)


#3. SVM
library('e1071')

model<-svm(formula= Species ~ . ,
           data = training_set,
           type='C-classification',
           kernel='linear', probability= TRUE)

y<- predict(model, test_set[-5], probability = TRUE)
pred<-predict(model, test_set[-5])
cm<-table(test_set[,5],pred)

head(attr(y, "probabilities"))

ypred<-predict(model,test_set[-5])
ypred<- ifelse(ypred>0.5,1,0)


#4. Decision Tree
library('rpart')
#Doing pre-pruning
model<- rpart(formula = Species ~ .,
              data = training_set, 
              method='class', control = rpart.control(cp=0,maxdepth = 8))

Speciespred<-predict(model, test_set[-5],type='class')

summary(model)
#Post-Pruning based on the complexity parameter(CP)if CP is high than the mentioned, 
#tree will be discarded.

model_pruned<-prune(model, cp=0.001)

base_accuracy<- mean(ypred == test_set$Species)

model$variable.importance

model$frame

plot(model)
printcp(model)
plotcp(model)
plot(text)

#5.NaiveBayes

model<-naiveBayes(x=training_set[-5],y=training_set[5],formula= Species~.)  
ypred<-predict(model, test_set[-5], type='class')
           
sd()

sd((100+120+130+125))


#Confusion matrix
cm<-table(test_set[,5], ypred)



