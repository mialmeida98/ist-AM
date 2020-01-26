#Esta é a minha parte do código. Uso como ponto de partida um dataframe que se chama raw_whitewines, e que
#é só mesmo o load do dataset
raw_whitewines <- read.table(file = "D:/Faculdade/MECD/1º Ano, 1º Semestre/Analise Multivariada/Project/winequality-white.csv",sep=";",
                             header=T,quote='')     
raw_redwines <- read.table(file = "D:/Faculdade/MECD/1º Ano, 1º Semestre/Analise Multivariada/Project/winequality-red.csv",sep=";",
                           header=T,quote='')

###########################################Outlier detection##############################################

#We start by standardizing the variables. Otherwise, the outlier detection might be skewed towards considering
#Only the variables that have values with higher degrees of magnitude.
stand_raw_whitewines <- raw_whitewines
stand_raw_whitewines[,1:11] <- scale(stand_raw_whitewines[,1:11])

stand_raw_redwines <- raw_redwines
stand_raw_redwines[,1:11] <- scale(stand_raw_redwines[,1:11])  

#Now first treat the white wine dataset
white_outlier_indexes <- c()
#Using a cutting value of 0.9999, and a method based on robust PCA's, the following cycle will, CLASS BY CLASS,
#flag the points that will be considered outliers, and register their index for later removal. It will also
#print how many (robust) PC's it used for each class
for(q in unique(stand_raw_whitewines$X.quality.)){
  rpca <- PcaHubert(stand_raw_whitewines[stand_raw_whitewines$X.quality. == q,1:11],crit.pca.distances = 0.9999)
  white_outlier_indexes <- c(white_outlier_indexes,as.integer(names((outl.rpca = which(rpca@flag==FALSE)))))
  print(paste("For quality",q,"the algorithm used",slot(rpca,"k"),"robust PC's"))
}

#Now analyzing the points that will be removed.
print(paste("The number of outliers flagged was",length(white_outlier_indexes)))

white_outlier_qcounts <- table(raw_whitewines[white_outlier_indexes,"X.quality."]); white_outlier_qcounts
white_full_qcounts <- table(raw_whitewines[,"X.quality."])
for (name in names(white_outlier_qcounts)){
  print(paste("For quality",name,"the number of points considered as outliers and removed was",
              white_outlier_qcounts[[name]],"which corresponds to a fraction of",
              round(white_outlier_qcounts[[name]]/white_full_qcounts[[name]],3),"of all points of that quality class"))
}

#Now, to finalize, we remove the points flagged as outliers from the dataset.
filtered_white <- raw_whitewines[-white_outlier_indexes,]
write.table(filtered_white, file="RPCAoutlier_whitewine.csv",row.names = FALSE,col.names=TRUE,sep=";")



#Now repeat the same process for the red wine
red_outlier_indexes <- c()
#Using a cutting value of 0.9999, and a method based on robust PCA's, the following cycle will, CLASS BY CLASS,
#flag the points that will be considered outliers, and register their index for later removal. It will also
#print how many (robust) PC's it used for each class
for(q in unique(stand_raw_redwines$X.quality.)){
  rpca <- PcaHubert(stand_raw_redwines[stand_raw_redwines$X.quality. == q,1:11],crit.pca.distances = 0.9999)
  red_outlier_indexes <- c(red_outlier_indexes,as.integer(names((outl.rpca = which(rpca@flag==FALSE)))))
  print(paste("For quality",q,"the algorithm used",slot(rpca,"k"),"robust PC's"))
}

#Now analyzing the points that will be removed.
print(paste("The number of outliers flagged was",length(red_outlier_indexes)))

red_outlier_qcounts <- table(raw_redwines[red_outlier_indexes,"X.quality."]); red_outlier_qcounts
red_full_qcounts <- table(raw_redwines[,"X.quality."])
for (name in names(red_outlier_qcounts)){
  print(paste("For quality",name,"the number of points considered as outliers and removed was",
              red_outlier_qcounts[[name]],"which corresponds to a fraction of",
              round(red_outlier_qcounts[[name]]/red_full_qcounts[[name]],3),"of all points of that quality class"))
}

#Now, to finalize, we remove the points flagged as outliers from the dataset.
filtered_red <- raw_redwines[-red_outlier_indexes,]
write.table(filtered_red, file="RPCAoutlier_redwine.csv",row.names = FALSE,col.names=TRUE,sep=";")






###########################«Clustered data comparative variable analysis boxplots########################

#Este código pega no dataframe chamado "red_selection", faz o clustering e depois faz os gráficos. Quando
#se juntar com o resto do código, pode-se cortar a parte de faezr o clustering, suponho eu
red_selection <- read.table(file = "D:/Faculdade/MECD/1º Ano, 1º Semestre/Analise Multivariada/Project/red_selection.csv",sep=";",header=T,quote='')
red_selection$quality = NULL
red_clusters = hkmeans(red_selection, 2, hc.metric = "euclidean", hc.method = "ward.D2")
red_selection$cluster = as.factor(red_clusters$cluster)

#Starting with red wine

#Here we standardize the variables, so as to make them visually comparable in the same plot. If we don't
#we would only see a blob of points clumped up together for most variables, since the scale would be defined
#by the one with higher values
stand_red_selection <- red_selection
stand_red_selection[,1:8] <- scale(stand_red_selection[,1:8])

#Here we draw the box plot
stand_red_selection.m <- melt(stand_red_selection, id.var = "cluster")
ggplot(data = stand_red_selection.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cluster)) +
  labs(title="Red Wine clustered data variable comparative analysis", x ="Wine Variables", y = "Standardized variable values") + 
  theme(plot.title = element_text(hjust = 0.5))



#Now the white wine:

white_selection <- read.table(file = "D:/Faculdade/MECD/1º Ano, 1º Semestre/Analise Multivariada/Project/white_selection.csv",sep=";",header=T,quote='')
white_selection$quality = NULL
white_clusters = hkmeans(white_selection, 2, hc.metric = "euclidean", hc.method = "ward.D2")
white_selection$cluster = as.factor(white_clusters$cluster)

stand_white_selection <- white_selection
stand_white_selection[,1:8] <- scale(stand_white_selection[,1:8])

stand_white_selection.m <- melt(stand_white_selection, id.var = "cluster")
ggplot(data = stand_white_selection.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cluster)) +
  labs(title="White Wine clustered data variable comparative analysis", x ="Wine Variables", y = "Standardized variable values") + 
  theme(plot.title = element_text(hjust = 0.5))