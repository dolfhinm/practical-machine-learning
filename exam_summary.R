#install package library

packages <- c("caret", "rpart", "gbm", "lubridate", "randomForest", "lda",
              "ggplot2", "nnet")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(caret); library(rpart); library(ggplot2)
library(gbm); library(lubridate); library(randomForest)
library(lda); library(nnet)

#download file
set.seed(1234)

url_train<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url_train,"pml-training.csv")
download.file(url_test,"pml-testing.csv")

data_train<-read.csv2("pml-training.csv", sep = ",", na.strings = c("NA", "#DIV/0!", ""))
data_test<-read.csv2("pml-testing.csv", sep = ",", na.strings = c("NA", "#DIV/0!", ""))

#clean data
data_train <- data_train[, colSums(is.na(data_train))==0]
data_test <- data_test[, colSums(is.na(data_test))==0]

dim(data_train)

data_train <- data_train[, -(1:7)]
data_test <- data_test[, -(1:7)]

#split train data on 60% on training and 40% on testing
inTrain <- createDataPartition(data_train$classe, p=0.6)[[1]]
data_train_training <- data_train[inTrain,]
data_train_testing <- data_train[-inTrain,]

data_train_training$classe = as.factor(data_train_training$classe)
data_train_testing$classe = as.factor(data_train_testing$classe)

dim(data_train_training)

plot(data_train_training$classe)

# clear memory 
gc()

#create model
#All models, besides Random Forest, have had error with memory or have very more time, so work have run with only one method
#tc <- trainControl(method = "cv", number = 1, verboseIter = FALSE)
#mod_gbm <- train(classe ~., data = data_train_training, method = "gbm", trControl = tc)
#mod_lda <- train(classe~., data = data_train_training, method = "lda", trControl = tc)
#mod_nn <- train(classe~., data = data_train_training, method = "nnet", trControl = tc)
#mod_las <- train(classe~., data = data_train_training, method = "lasso", trControl = tc)

# model Decision Tree
mod_rf2 <- randomForest(data_train_training$classe ~ ., data=data_train_training) 

predTrain <- predict(mod_rf2, data_train_training)
confusionMatrix(predTrain, data_train_training$classe)

predTrainTest <- predict(mod_rf2, data_train_testing)
confusionMatrix(predTrainTest, data_train_testing$classe)

predTest <- predict(mod_rf2, data_test)
predTest
