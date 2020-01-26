#CLUSTERING ANALYSIS
library(factoextra)
library(stats)
library(cluster)
library(GGally)
library(plotly)

#WHITE WINE
#hierarchical clustering
w_h1<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "single", keep.data = FALSE)
w_h2<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "complete", keep.data = FALSE)
w_h3<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "average", keep.data = FALSE)
w_h4<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "ward", keep.data = FALSE)

#dendrograms
par(mfrow=c(2,2))
pltree(r_h1,main="Single linkage", cex=0.83,xlab="")
pltree(r_h2,main="Complete linkage",cex=0.83,xlab="")
pltree(r_h3,main="Average linkage", cex=0.83,xlab="")
pltree(r_h4,main="Ward Method", cex=0.83,xlab="")#2 or 4 clusters

#average silhouette width
silh_hkmeans = fviz_nbclust(winequalitywhite, FUNcluster = kmeans, method="silhouette")
print(silh_hkmeans)#2

#Hierarchical K-Means (considering the previous results)
hk_w = hkmeans(winequalitywhite, 2, hc.metric = "euclidean", hc.method = "ward.D2")
hk_w2 = hkmeans(winequalitywhite, 4, hc.metric = "euclidean", hc.method = "ward.D2")
print(hk_w)
print(hk_w2)

#Hierarchical K-Means Standardized
white_stand = scale(winequalitywhite)
hk_w_s = hkmeans(white_stand, 2, hc.metric = "euclidean", hc.method = "ward.D2")
print(hk_w_s) #way worse result

#K-Medoids
#Determine the Optimal Number of Clusters
silh_m = fviz_nbclust(winequalitywhite, FUNcluster = cluster::pam, method="silhouette")
print(silh_m)#2

#K-Medoids (tried with Manhattan, worse result)
kmedoids_white = pam(winequalitywhite,k=2,metric="euclidean",stand=FALSE)
print(kmedoids_white)
kmedoids_white$silinfo #0.567

##########################STANDARDIZED#############################################
#Hierarchical K-Means
w_h1s<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "single", keep.data = TRUE)
w_h2s<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "complete", keep.data = TRUE)
w_h3s<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "average", keep.data = TRUE)
w_h4s<-agnes(winequalitywhite, metric = "euclidean",
            stand = FALSE, method = "ward", keep.data = TRUE)

#dendrograms
par(mfrow=c(2,2))
pltree(w_h1s,main="Single linkage", cex=0.83,xlab="")
pltree(w_h2s,main="Complete linkage",cex=0.83,xlab="")
pltree(w_h3s,main="Average linkage", cex=0.83,xlab="")
pltree(w_h4s,main="Ward Method", cex=0.83,xlab="")#2 or 4 clusters

#compute the algorithm
white_stand = scale(winequalitywhite)
hk_w_s = hkmeans(white_stand, 2, hc.metric = "euclidean", hc.method = "ward.D2")
print(hk_w_s) #worse result

#K-Medoids
silh_m_s = fviz_nbclust(white_stand, FUNcluster = cluster::pam, method="silhouette")
print(silh_m__s)#2
kmedoids_white_s = pam(winequalitywhite,k=2,metric="euclidean",stand=TRUE)
print(kmedoids_white_s)
kmedoids_white_s$silinfo #worse result
###################################################################################

#Compare the results without standardization
kmedoids_white$silinfo
silh_hkmeans$data



#RED WINE
#hierarchical clustering
r_h1<-agnes(winequalityred, metric = "euclidean",
            stand = FALSE, method = "single", keep.data = FALSE)
r_h2<-agnes(winequalityred, metric = "euclidean",
            stand = FALSE, method = "complete", keep.data = FALSE)
r_h3<-agnes(winequalityred, metric = "euclidean",
            stand = FALSE, method = "average", keep.data = FALSE)
r_h4<-agnes(winequalityred, metric = "euclidean",
            stand = FALSE, method = "ward", keep.data = FALSE)

#dendrograms
par(mfrow=c(2,2))
pltree(r_h1,main="Single linkage", cex=0.83,xlab="")
pltree(r_h2,main="Complete linkage",cex=0.83,xlab="")
pltree(r_h3,main="Average linkage", cex=0.83,xlab="")
pltree(r_h4,main="Ward Method", cex=0.83,xlab="")#2 or 4 clusters

#average silhouette width
silh_hkmeans_red = fviz_nbclust(winequalityred, FUNcluster = kmeans, method="silhouette")
print(silh_hkmeans_red)#2

#Hierarchical K-Means (considering the previous results)
hk_w = hkmeans(winequalityred, 2, hc.metric = "euclidean", hc.method = "ward.D2")
hk_w2 = hkmeans(winequalityred, 4, hc.metric = "euclidean", hc.method = "ward.D2")
print(hk_w)
print(hk_w2)

#K-Medoids
#Determine the Optimal Number of Clusters
silh_m_r = fviz_nbclust(winequalityred, FUNcluster = cluster::pam, method="silhouette")
print(silh_m_r)#2

#K-Medoids (tried with Manhattan, worse result)
kmedoids_red = pam(winequalityred,k=2,metric="euclidean",stand=FALSE)
print(kmedoids_red)
kmedoids_red$silinfo


##########################STANDARDIZED#############################################
#Hierarchical K-Means
r_h1s<-agnes(winequalityred, metric = "euclidean",
             stand = FALSE, method = "single", keep.data = TRUE)
r_h2s<-agnes(winequalityred, metric = "euclidean",
             stand = FALSE, method = "complete", keep.data = TRUE)
r_h3s<-agnes(winequalityred, metric = "euclidean",
             stand = FALSE, method = "average", keep.data = TRUE)
r_h4s<-agnes(winequalityred, metric = "euclidean",
             stand = FALSE, method = "ward", keep.data = TRUE)

#dendrograms
par(mfrow=c(2,2))
pltree(r_h1s,main="Single linkage", cex=0.83,xlab="")
pltree(r_h2s,main="Complete linkage",cex=0.83,xlab="")
pltree(r_h3s,main="Average linkage", cex=0.83,xlab="")
pltree(r_h4s,main="Ward Method", cex=0.83,xlab="")#2 or 4 clusters

#compute the algorithm
red_stand = scale(winequalityred)
hk_w_s = hkmeans(red_stand, 2, hc.metric = "euclidean", hc.method = "ward.D2")
print(hk_w_s) #worse result

#K-Medoids
silh_m_r_s = fviz_nbclust(red_stand, FUNcluster = cluster::pam, method="silhouette")
print(silh_m_r_s)#2
kmedoids_red_s = pam(winequalityred,k=2,metric="euclidean",stand=TRUE)
print(kmedoids_red_s)
kmedoids_red_s$silinfo #worse result
####################################################################################

#Compare the results without standardization
kmedoids_red$silinfo
silh_hkmeans_red$data

####################################################################################
#Analyse Hierarchical K-means on the best dataset --> after feature selection
#RED WINE
red_hkmeans = red_selection
red_hkmeans$quality = NULL
red_clusters = hkmeans(red_hkmeans, 2, hc.metric = "euclidean", hc.method = "ward.D2")
red_selection$cluster = as.factor(red_clusters$cluster)

#First Look
print(red_clusters)
red_clusters$betweenss
red_clusters$centers

#Summary Statistics
summary(red_selection[red_selection$cluster== 1,])
summary(red_selection[red_selection$cluster== 2,])

#Plot
plot_red = ggparcoord(data = red_selection, columns = c(1:8), groupColumn = "cluster", scale = "std", alphaLines = 0.3)+ labs(x = "Red Wine Features", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(plot_red)


#WHITE WINE
white_hkmeans = white_selection
white_hkmeans$quality = NULL
white_clusters = hkmeans(white_hkmeans, 2, hc.metric = "euclidean", hc.method = "ward.D2")
white_selection$cluster = as.factor(white_clusters$cluster)

#First Look
print(white_clusters)
white_clusters$betweenss
white_clusters$centers

#Summary Statistics
summary(white_selection[white_selection$cluster== 1,])
summary(white_selection[white_selection$cluster== 2,])

#Plot
plot_white = ggparcoord(data = white_selection, columns = c(1:8), groupColumn = "cluster", scale = "std", alphaLines = 0.3)+ labs(x = "White Wine Features", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(plot_white)

