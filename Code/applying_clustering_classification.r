library(randomForest)
library(rpart)

#Applying the Results

#RED WINE
#Split the Dataset
library(caTools)
set.seed(123)
split_cluster_red = sample.split(red_selection, SplitRatio = 0.7)
training_cluster = subset(red_selection, split_r == TRUE)
test_cluster = subset(red_selection, split_r == FALSE)
red_selection$quality = NULL

#Hierarchical K-Means
#Training
red_train = hkmeans(training_cluster, 2, hc.metric = "euclidean", hc.method = "ward.D2")
red_train#66.6
red_svm_cluster = training_cluster
red_svm_cluster$cluster = as.factor(red_train$cluster)
write.table(red_svm_cluster, "~/Desktop/red_training.csv⁩",sep=",")

#Get the label on testing
red_test = hkmeans(test_cluster, 2, hc.metric = "euclidean", hc.method = "ward.D2")
red_test#70.4
red_svm_cluster_test = test_cluster
red_svm_cluster_test$cluster = as.factor(red_test$cluster)
write.table(red_svm_cluster_test, "~/Desktop/red_testing.csv⁩",sep=",")

#RANDOM FOREST AND DECISION TREE
training_cluster$cluster <- as.factor(training_cluster$cluster)
testing_cluster$cluster <- as.factor(testing_cluster$cluster)

set.seed(10)
form <- as.formula("cluster ~.")

#Decision Tree
tmp.model.t<-rpart(form, data = training_cluster, method = "class")
tmp.predict.t <-predict(tmp.model.t, newdata = testing_cluster[,1:8], type="class")
conf.mat.t <- table(Predict=tmp.predict.t, True=testing_cluster$cluster)
print(conf.mat.t)
acc_t <- mean(tmp.predict.t==testing_cluster$cluster)
acc_t

#Random Forest
tmp.model.rf <- randomForest(form, data = training_cluster)
tmp.predict.rf <- predict(tmp.model.rf, newdata=testing_cluster[,1:8], type="class")
conf.mat.rf <- table(Predict=tmp.predict.rf, True=testing_cluster$cluster)
print(conf.mat.rf)
acc_rf <- mean(tmp.predict.rf==testing_cluster$cluster)
acc_rf


#WHITE WINE
#Split the Dataset
library(caTools)
set.seed(123)
split_cluster_white = sample.split(white_selection, SplitRatio = 0.7)
training_cluster_white = subset(white_selection, split_cluster_white == TRUE)
test_cluster_white = subset(white_selection, split_cluster_white == FALSE)
white_selection$quality = NULL

#Hierarchical K-Means
#Training
white_train = hkmeans(training_cluster_white, 2, hc.metric = "euclidean", hc.method = "ward.D2")
white_train#59.8
white_svm_cluster = training_cluster_white
white_svm_cluster$cluster = as.factor(white_train$cluster)
write.table(white_svm_cluster, "~/Desktop/clustering_white_training.csv⁩",sep=",")

#Get the label on testing
white_test = hkmeans(test_cluster_white, 2, hc.metric = "euclidean", hc.method = "ward.D2")
white_test#62.3
white_svm_cluster_test = test_cluster_white
white_svm_cluster_test$cluster = as.factor(white_test$cluster)
write.table(white_svm_cluster_test, "~/Desktop/clustering_white_testing.csv⁩",sep=",")

#SVM
#feature scaling
white_svm_cluster[,1:8] = scale(white_svm_cluster[,1:8])
white_svm_cluster_test[,1:8] = scale(white_svm_cluster_test[,1:8])

#fitting svm to the training set
library(e1071)
cluster_svm_white = svm(formula=cluster ~., data=white_svm_cluster, type = 'C-classification', kernel = 'radial')

#predicting the test set
pred_cluster_white = predict(cluster_svm_white, newdata = white_svm_cluster_test[,1:8])

#confusion matrix
conf_matrixwhite = table(white_svm_cluster_test[,9], pred_cluster_white)
conf_matrixwhite
accuracy_white = (conf_matrixwhite[1,1]+conf_matrixwhite[2,2])/sum(conf_matrixwhite)
accuracy_white
