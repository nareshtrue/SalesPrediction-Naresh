data<- read.csv('C:\\Users\\v-gimupp\\Desktop\\train\\train.csv')


#Creating domain type Variable
x<-list()
require(stringi)
for (i in 1:nrow(data)){
  x[i]<-substr(data[i,2], 
            start=unlist(gregexpr(pattern ='.',data[i,2], fixed = T))[length(unlist(gregexpr(pattern ='.',data[i,2], fixed = T)))]+1,
            stop = stri_length(data[i,2]))
}
data$DomainType<-unlist(x)

#Pulling substring that comes between 3 accurance and 4 accurance  of '/'
Label_2<- list()
for (i in 1:nrow(data)){
  Label_2[i]<-substr(data[i,3], 
                     unlist(gregexpr(pattern ='/',data[i,3]))[3]+1,
                     unlist(gregexpr(pattern ='/',data[i,3]))[4]-1)
  
}
#Unlisting the Label2 in order to bind this to original data frame.
Label2<-unlist(Label_2)

#Binding the newlpy created label from the 
#url to the existing data after removing URL column

data<- cbind.data.frame(data,Label2)

#Pull the unique rows from the pre-processed data frame 'data'

Unique_Data<- unique(data[,-c(1,2,3)])

#Removing Alpha-Numerics from the 
#string value of Label_2 from the Unique_data freame
y <- Unique_Data[,3]
 x<-list()
 for (i in 1:length(y)){
   x[i]<- c(as.character(gsub("[0-9]+", "", y[i])),x)

 }
 
 RemovedAlphanumericLabel<- unlist(x)

Unique_Data$PreprocessedLabel2<-RemovedAlphanumericLabel

#Removing Label 2 from Unique_Data dataframe
#Unique_Data1<-Unique_Data[,-3]

#Assigning the NA to the empty values that came 
#beacuase existing only numerical values
Unique_Data$PreprocessedLabel2[Unique_Data$PreprocessedLabel2==""]<-NA
Unique_Data$PreprocessedLabel2[is.na(Unique_Data$PreprocessedLabel2)==TRUE]<-"Other"
Unique_Data2<- unique(Unique_Data[,-3])

#Labelling for Preprocessing label

#levels<-as.vector(unique(Unique_Data2$PreprocessedLabel2))
#Unique_Data2$PreprocessedLabel2<-factor(Unique_Data2$PreprocessedLabel2, 
 #                           levels=levels,
  #                          labels  = as.vector(seq(1,length(levels), 1)))


#Labelling Domain Variale
#level<-as.vector(unique(Unique_Data2$Domain))
#Unique_Data2$Domain<-factor(Unique_Data2$Domain, 
 #                                       levels=level,
  #                                      labels  = as.vector(seq(1,length(level), 1)))

                            
                            



#unique(Unique_Data2)
#library('xlsx')


#for (i in 1:nrow(Unique_Data2)){
  #Unique_Data2[i,3] <- gsub("[/,@,#,%,-]"," ","Unique_Data2[i,3]") 
  
#}



#Str
#str(Unique_Data2)
Unique_Data2$DomainType<- as.factor(Unique_Data2$DomainType)
Unique_Data2$PreprocessedLabel2<- as.factor(Unique_Data2$PreprocessedLabel2)
#splitting test and train
sample<-sample(nrow(Unique_Data2), nrow(Unique_Data2)*0.75)
train<- Unique_Data2[sample,]
test<- Unique_Data2[-sample,]
for (i in nrow(Unique_Data2)){
  if (stri_length(Unique_Data2$PreprocessedLabel2[i])>50){
    Unique_Data2$PreprocessedLabel2[i]<-"FullLenght"
  }
    
}
stri_length(Unique_Data2$PreprocessedLabel2[4])>20
str(Unique_Data2)
str(Unique_Data2)
#Building the tree.
#library(randomForest)
#rf<-randomForest(Tag~., data = train, mtry=3, ntree=20)


library(rpart)
dt<-rpart(Tag~., data = train)