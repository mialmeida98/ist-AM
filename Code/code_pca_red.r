data=read.table("winequality-red.csv",header=TRUE,sep=";")

# Robust PCA
pca <- PcaCov(data[,1:11], scale=TRUE)
summary (pca)

# Loadings
loadings<-pca$loadings
print(round(loadings , digits = 3))

# Screeplot 
screeplot(pca, main="Robust PCA based on the Standardized Variables", type="lines",cex=1,lwd=2)
abline(h=1,col="orange",lwd=2)
