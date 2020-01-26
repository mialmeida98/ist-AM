#--------------------------------------------------------------------
# Linear Discriminant Analysis
#--------------------------------------------------------------------

wine=read.table("winequality-white.csv",header=TRUE,sep=";")

str(wine)
summary(wine)
plot(prop.table(table(wine$quality)))


table(wine$quality)
round(prop.table(table(wine$quality)),3)*100


set.seed(123)
#library("MASS")


split_r = sample.split(wine$quality, SplitRatio = 0.75)
training_set = subset(wine, split_r == TRUE)
test_set = subset(wine, split_r == FALSE)


wine.lda = lda(formula = wine$quality ~ 
                 wine$fixed.acidity        + wine$volatile.acidity + wine$citric.acid 
               + wine$residual.sugar       + wine$chlorides        + wine$free.sulfur.dioxide 
               + wine$total.sulfur.dioxide + wine$density          + wine$pH 
               + wine$sulphates            + wine$alcohol,
               data = training_set)



wine.lda$scaling

wine.lda.values = predict(wine.lda, test_set[1:11])

par(mar=c(1,1,1,1))

# histogramas sobrepostos => as variáveis não discriminam linearmente a variável resposta (qualidade)
ldahist(data = wine.lda.values$x[,1], g=wine$quality)
ldahist(data = wine.lda.values$x[,2], g=wine$quality)
ldahist(data = wine.lda.values$x[,3], g=wine$quality)
ldahist(data = wine.lda.values$x[,4], g=wine$quality)
ldahist(data = wine.lda.values$x[,5], g=wine$quality)
ldahist(data = wine.lda.values$x[,6], g=wine$quality)

plot(wine.lda.values$x[,1],wine.lda.values$x[,2],xlab = "LD1",ylab = "LD2",
     col=wine$quality,pch=16)

text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$quality,
     cex=0.7,pos=4,col=wine$quality)
