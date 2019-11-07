library(keras)
install_keras()
library(caret)
library(readr)
library(dplyr)
library(rnn)
library(tensorflow)
library(rsample)
library(MLmetrics)
library(magrittr)
library(tidyverse)


data <- read.csv(file.choose(),header= T)
#Setting outcome variables as categorical

data<-data[ , apply(data, 2, var) != 0]

data$Class <- factor(data$Class)


#Randomly shuffle the data
data<-data[sample(nrow(data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)


#Perform 10 fold cross validation
for(i in 1:10)
{

    #Segement your data by fold using the which() function 
    testIndexes<- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]


x_train <- trainData[ ,1:119] %>% as.matrix()
y_train <- trainData[ ,120,drop = TRUE] %>% as.matrix()
dmy <- dummyVars(" ~ .", data = x_train)
trsf <- as.matrix(predict(dmy, newdata = x_train))
dmy1 <- dummyVars(" ~.", data = y_train)
trsf1 <- as.matrix(predict(dmy1, newdata = y_train))


model<-keras_model_sequential() %>%   
  layer_dense(units=512,activation = "tanh",input_shape = 119) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 256, activation = "tanh") %>%
  layer_dropout(rate = 0.2) %>%
    layer_dense(units = 200, activation = "tanh") %>%
   layer_dropout(rate = 0.2) %>%
 layer_dense(units = 120, activation = "tanh") %>%
   layer_dropout(rate = 0.2) %>%
 layer_dense(units = 90, activation = "tanh") %>%
   layer_dropout(rate = 0.2) %>%
 layer_dense(units = 50, activation = "tanh") %>%
   layer_dropout(rate = 0.2) %>%
 layer_dense(units = 37 , activation = "softmax")

keras_compile<- compile(model,
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),
  metrics = c('accuracy'))


batch_size = 50
epochs = 70
validation_split=0.2

rnn_history <-fit(model,trsf ,trsf1 , batch_size = batch_size,view_metrics=TRUE,
  epochs = epochs,validation_split=0.2)

summary(model)

x_test <- testData[ ,1:119] %>% as.matrix()
y_test <- testData[ ,120,drop = FALSE] %>% as.matrix()
dmy3<- dummyVars(" ~ .", data = x_test)
trsf2 <- as.matrix(predict(dmy3, newdata = x_test))
dmy2 <- dummyVars(" ~ .", data = y_test)
trsf3 <- as.matrix(predict(dmy2, newdata = y_test))
##prediction 
pred <- model %>% predict(trsf2, batch_size = 128)
Y_pred = round(pred)
# Confusion matrix
CM = table(Y_pred, trsf3)

# evaluate the model
evals <- model %>% evaluate(x_test, y_test, batch_size = 50)

accuracy = evals[2][[1]]* 100
}


# New list
l <- list();
l1 <- list();

x_test <- testData[ ,1:119] %>% as.matrix()
y_test <- testData[ ,120,drop = FALSE] %>% as.matrix()
dmy3<- dummyVars(" ~ .", data = x_test)
trsf2 <- as.matrix(predict(dmy3, newdata = x_test))
dmy2 <- dummyVars(" ~ .", data = y_test)
trsf3 <- as.matrix(predict(dmy2, newdata = y_test))
score<-model %>% evaluate(trsf2,trsf3)
Pred<-model %>% predict_classes(trsf2)
 l <- c(l,score)
 l1 <- c(l1,Pred)