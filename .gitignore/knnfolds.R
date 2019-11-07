library(caret)
library(plyr)
library(magrittr)
library(psych)
library(MLmetrics)
library(e1071)
library(class)
#IMPORTING THE DATA
data<-read.csv(file.choose(),header=T)

#REMOVE ZERO VARIANCE COLUMNS
data<-data[ , apply(data, 2, var) != 0]
data<-na.omit(data)
#CONVERT CLASS TO FACTOR
data$Class <- factor(data$Class)
data<-data[1:50000, ]
dim(data)
str(data$Class)
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
data[,1:18]<- normalize(data[,1:18]) 



set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(data$Class,k = k,list = TRUE,returnTrain = TRUE)
    for (i in 1:k) {
  
    traindata<-(data[folds[[i]],])[-19]
    newdata<-(data[-folds[[i]],])[-19]
    

    model <- knn(traindata,newdata,data[folds[[i]],]$Class,k =700,l = 0, prob = FALSE, use.all = FALSE)
        
       
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(model, data[-folds[[i]], ]$Class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(model, data[-folds[[i]], ]$Class)$byClass[1:8,7])))

    }
    accuracies.dt
    f1.dt
}

accuracies.dt<-c()
f1.dt <- c()
accuracies.dt<-k.folds(10)
f1.dt <- k.folds(10)
f1.dt
accuracies.dt
