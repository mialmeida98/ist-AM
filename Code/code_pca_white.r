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
library(rospca)

white_wine = read.csv("winequality-white.csv", header=TRUE, sep = ";")

#PCA Robusta com standardização
rob.pca.white<-PcaCov(white_wine[,0:11], scale = TRUE) #Normalizada

screeplot(rob.pca.white, main="", type="lines")
abline(h=1.0^2, col="yellow")
summary(rob.pca.white)
