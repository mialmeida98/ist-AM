#FEATURE SELECTION
library(mRMRe)

#Red Wine Without Outliers
RPCAoutlier_redwine$X.quality. = factor(RPCAoutlier_redwine$X.quality., levels = c(0,1,2,3,4,5,6,7,8,9,10), ordered = TRUE)
RPCAoutlier_redwine$X.free.sulfur.dioxide. = as.numeric(as.character(RPCAoutlier_redwine$X.free.sulfur.dioxide.))

#Transform into an MRMR Dataset
dataset_outliers = mRMR.data(RPCAoutlier_redwine)

#MRMR Algorithm
feat_sel = mRMR.classic(data = dataset_outliers, target_indices = 12, feature_count = 11)
feat_sel@filters
feat_sel@scores
solutions(feat_sel)

#White Wine Without Outliers
RPCAoutlier_whitewine$X.quality. = factor(RPCAoutlier_whitewine$X.quality., levels = c(0,1,2,3,4,5,6,7,8,9,10), ordered = TRUE)

#Transform into an MRMR Dataset
dataset_outliers_white = mRMR.data(RPCAoutlier_whitewine)

#MRMR Algorithm
feat_sel_white = mRMR.classic(data = dataset_outliers_white, target_indices = 12, feature_count = 11)
feat_sel_white@filters
feat_sel_white@scores
solutions(feat_sel_white)


#----------------------------------------------#
#Red Wine With Outliers
winequalityred$quality = factor(winequalityred$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10), ordered = TRUE)

dataset_red = mRMR.data(winequalityred)

feat_sel_red = mRMR.classic(data = dataset_red, target_indices = 12, feature_count = 11)
feat_sel_red@filters
feat_sel_red@scores
feat_sel_red@feature_names

#White Wine With Outliers
winequalitywhite$quality = factor(winequalitywhite$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10), ordered = TRUE)

dataset_white = mRMR.data(winequalitywhite)

feat_sel_whiteo = mRMR.classic(data = dataset_white, target_indices = 12, feature_count = 11)
feat_sel_whiteo@filters
feat_sel_whiteo@scores
feat_sel_whiteo@feature_names

