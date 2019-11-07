library(caret)
library(plyr)
library(magrittr)
library(psych)
library(MLmetrics)
library(e1071)
library(rpart)
#IMPORTING THE DATA
data<-read.csv(file="C:/Users/shilp/Downloads/DatasetsAML/Datasets/Gear23.csv", header=TRUE, sep=",")

#REMOVE ZERO VARIANCE COLUMNS
data<-data[ , apply(data, 2, var) != 0]

#CONVERT CLASS TO FACTOR
data$class <- factor(data$class)
dim(data)
str(data$class)
 
#********

set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(data$class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {

   control <- rpart.control(minsplit = 4,
    minbucket = round(4/ 3),
    maxdepth = 25,
    cp = 0)
    model <- rpart(class~.,data=data[folds[[i]],] ,method = "class",control = control)
        
    predictions <- predict(object = model, newdata = (data[-folds[[i]],])[-118], type = "class")
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions, data[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions, data[-folds[[i]], ]$class)$byClass[1:40,7])))

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
