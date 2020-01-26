library(ggplot2)
library (gridExtra)
library(GGally)
library(MASS)
library(plyr)
library(psych)
library(rpca)
library(rrcov)
library(normalr)
library(readxl)
library(ISLR)
library(tree)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(plyr)
library(caret)
library(e1071)


red_wine = read.csv("winequality-red.csv", header=TRUE, sep = ";")

set.seed(10)
form <- as.formula("quality ~.")
folds <- split(red_wine, cut(sample(1:nrow(red_wine)),10))
acc_t <- rep(NA, length(folds))
acc_f <- rep(NA, length(folds))
#tmp.predict_t. <- matrix(NA, ncol = 10)
metric <- "Accuracy"


#Decision tree
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame) #Split data into test and train
  test$.id=NULL #Remove column
  train <- ldply(folds[-i], data.frame)
  train$.id=NULL
  tmp.model_t <- rpart(form , data=train, method = "class") #Decision tree model
  tmp.predict_t <- predict(tmp.model_t, newdata = test[,1:11], type = "class")
  #tmp.predict_t.[,i] <-cbind(tmp.predict_t)
  conf.mat_t <- table(tmp.predict_t, true=test$quality)
  print(conf.mat_t)
  acc_t[i] <- mean(tmp.predict_t==test$quality)
  rpart.plot(tmp.model_t)
}
 print(sprintf("average accuracy using k-fold cross-validation is= %.3f percent", 100*mean(acc_t)))


for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame) #Split data into test and train
  test$.id=NULL #Remove column
  test$quality <- as.character(test$quality)
  train <- ldply(folds[-i], data.frame)
  train$.id=NULL
  train$quality <- as.factor(train$quality)
  tmp.model_f <- randomForest(form, data = train) #Random forest model
  tmp.predict_f <- predict(tmp.model_f, newdata = test[,1:11], type="class")
  print(i)
  conf_mat_f<- table(tmp.predict_f, true=test$quality)
  print(conf_mat_f)
  acc_f[i] <- mean(tmp.predict_f==test$quality)
  
}

print(sprintf("average accuracy using k-fold cross-validation is= %.3f percent", 100*mean(acc_f)))

