options(warn=-1)


library(caret)
library(klaR)
library(psych)
library(MLmetrics)
library(mlbench)

#Reading data into R
data<- read.csv(file.choose(),header= T)
data <- na.omit(data)

#Studying the structure of the datstr(data)
head(data)
#to remove zero variance columns from the dataset
data<-data[ , apply(data, 2, var) != 0]

#Setting outcome variables as categorical
data$Class <- as.factor(data$Class)


#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = data$Class,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]
 
#Check dimensions of the split
 
prop.table(table(data$Class)) * 100
prop.table(table(training$Class)) * 100
prop.table(table(testing$Class)) * 100


set.seed(3033)



library(e1071)
model<-train(Class~.,data=training,method="nb",trControl=trainControl(method='cv',number=10,summaryFunction=multiClassSummary),search="random",tuneLength=10,alpha=1,distribution="multinomial")
 
model 

model$resample 

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing)
#Get the confusion matrix to see accuracy value and other parameter values
 
confusionMatrix(Predict,testing$Class)
