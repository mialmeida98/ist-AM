#EXPLORATORY ANALYSIS
library(psych)

#Red Wine
summary(winequalityred)
pairs.panels(winequalityred[1:11], ellipses = TRUE, cex.cor = 2)
var(winequalityred)
table(winequalityred$quality)


#White Wine
par(mfrow=c(1,1))
summary(winequalitywhite)
pairs.panels(winequalitywhite[1:11], ellipses = TRUE, cex.cor = 2)
var(winequalitywhite)
table(winequalitywhite$quality)
