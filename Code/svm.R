#SUPPORT VECTOR MACHINE

#WHITE WINE
#transforming the output variable into a factor
winequalitywhite$quality = factor(winequalitywhite$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10))

#splitting the dataset
library(caTools)
set.seed(123)
split_w = sample.split(winequalitywhite$quality, SplitRatio = 0.75)
training_set_white = subset(winequalitywhite, split_w == TRUE)
test_set_white = subset(winequalitywhite, split_w == FALSE)

#feature scalling
training_set_white[,1:11] = scale(training_set_white[,1:11])
test_set_white[,1:11] = scale(test_set_white[,1:11])

#fitting svm to the training set
library(e1071)
classifier_w = svm(formula=quality ~., data=training_set_white, type = 'C-classification', kernel = 'linear')

#predicting the test set
y_pred_w = predict(classifier_w, newdata = test_set_white[,1:11])

#first confusion matrix
conf_matrixwhite = table(test_set_white[,12], y_pred_w)

#10-fold cross validation
library(caret)
folds_w = createFolds(training_set_white$quality, k=10)
cross_v_w = lapply(folds_w, function(x){
  training_fold = training_set_white[-x,]; 
  test_fold=training_set_white[x,]; 
  svm_white=svm(formula=quality~., data=training_fold, type='C-classification', kernel='linear'); 
  y_svm_white=predict(svm_white, newdata=test_fold[,1:11]); 
  conf_matrix_white = table(test_fold[,12], y_svm_white); 
  accuracy_white = (conf_matrix_white[1,1]+conf_matrix_white[2,2]+conf_matrix_white[3,3]+conf_matrix_white[4,4]+conf_matrix_white[5,5]+conf_matrix_white[6,6]+conf_matrix_white[7,7]+conf_matrix_white[8,8]+conf_matrix_white[9,9]+conf_matrix_white[10,10]+conf_matrix_white[11,11])/(sum(conf_matrix_white)); 
  return(accuracy_white)
})

#compute the average accuracy
mean_white = array(as.numeric(unlist(cross_v_w)), dim=c(10,1))
mean(mean_white)


#RED WINE
#transforming the output variable into a factor
winequalityred$quality = factor(winequalityred$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10))

#splitting the dataset
library(caTools)
set.seed(123)
split_r = sample.split(winequalityred$quality, SplitRatio = 0.75)
training_set_red = subset(winequalityred, split_r == TRUE)
test_set_red = subset(winequalityred, split_r == FALSE)

#feature scalling
training_set_red[,1:11] = scale(training_set_red[,1:11])
test_set_red[,1:11] = scale(test_set_red[,1:11])

#fitting svm to the training set
library(e1071)
classifier_r = svm(formula=quality ~., data=training_set_red, type = 'C-classification', kernel = 'linear')

#predicting the test set
y_pred_r = predict(classifier_r, newdata = test_set_red[,1:11])

#first confusion matrix
conf_matrixred = table(test_set_red[,12], y_pred_r)

#10-fold cross validation
library(caret)
folds_r = createFolds(training_set_red$quality, k=10)
cross_v_r = lapply(folds_r, function(x){
  training_fold = training_set_red[-x,]; 
  test_fold=training_set_red[x,]; 
  svm_red=svm(formula=quality~., data=training_fold, type='C-classification', kernel='linear'); 
  y_svm_red=predict(svm_red, newdata=test_fold[,1:11]); 
  conf_matrix_red = table(test_fold[,12], y_svm_red); 
  accuracy_red = (conf_matrix_red[1,1]+conf_matrix_red[2,2]+conf_matrix_red[3,3]+conf_matrix_red[4,4]+conf_matrix_red[5,5]+conf_matrix_red[6,6]+conf_matrix_red[7,7]+conf_matrix_red[8,8]+conf_matrix_red[9,9]+conf_matrix_red[10,10]+conf_matrix_red[11,11])/(sum(conf_matrix_red)); 
  return(accuracy_red)
})

#compute average accuracy
mean_red = array(as.numeric(unlist(cross_v_r)), dim=c(10,1))
mean(mean_red)




