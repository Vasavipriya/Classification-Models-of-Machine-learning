library(caret)
library(plyr)
library(psych)
library(MLmetrics)
library(e1071)
library(kernlab)

#IMPORTING THE DATA
data<-read.csv(file.choose(), header=TRUE, sep=",")

#REMOVE ZERO VARIANCE COLUMNS
data<-data[ , apply(data, 2, var) != 0]

#CONVERT CLASS TO FACTOR
data$Class <- factor(data$Class)
dim(data)
str(data$Class)

rbf <- rbfdot(sigma=0.1)
rbf


set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(data$Class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
  
    model <- ksvm(Class~.,data=data[folds[[i]],],kernel="rbf",C =50,gamma=0.001,nu = 0.2, epsilon = 0.1, prob.model = FALSE,scale = TRUE)
        
        predictions <- predict(object = model, newdata = (data[-folds[[i]],])[-120])
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions, data[-folds[[i]], ]$Class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions, data[-folds[[i]], ]$Class)$byClass[1:37,7])))

    }
    accuracies.dt
    f1.dt
}

accuracies.dt<-c()
f1.dt <- c()
accuracies.dt<-k.folds(10)
f1.dt <- k.folds(10)
accuracies.dt
f1.dt
