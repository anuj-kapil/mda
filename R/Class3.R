wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep = ",")
colnames(wine) <- c("Cult", "Alc", "MalAcid", "Ash", "AshAlk", "Mag", "TotPhen",
                    "Flav", "NonFlav", "Proant", "Color", "Hue", "OD280OD315", "Proline")
library(ggplot2)
library(gridExtra)
p1 <- ggplot(wine, aes(x = wine$Cult)) + geom_density()
p2 <- ggplot(wine, aes(x = wine$Alc)) + geom_density()
p3 <- ggplot(wine, aes(x = wine$MalAcid)) + geom_density()
p4 <- ggplot(wine, aes(x = wine$Ash)) + geom_density()
p5 <- ggplot(wine, aes(x = wine$AshAlk)) + geom_density()
p6 <- ggplot(wine, aes(x = wine$Mag)) + geom_density()
p7 <- ggplot(wine, aes(x = wine$TotPhen)) + geom_density()
p8 <- ggplot(wine, aes(x = wine$Flav)) + geom_density()
p9 <- ggplot(wine, aes(x = wine$NonFlav)) + geom_density()
p10 <- ggplot(wine, aes(x = wine$Proant)) + geom_density()
p11 <- ggplot(wine, aes(x = wine$Color)) + geom_density()
p12 <- ggplot(wine, aes(x = wine$Hue)) + geom_density()
p13 <- ggplot(wine, aes(x = wine$OD280OD315)) + geom_density()
p14 <- ggplot(wine, aes(x = wine$Proline)) + geom_density()
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, ncol = 3)



par(mfrow = c(3, 3))
for (i in 1:9) {
  qqnorm(wine[, i], sub = colnames(wine)[i])
}

qqnorm(wine[, 3], sub = colnames(wine)[3])

apply(wine,2,function(x) ks.test(scale(x), y=pnorm))
?pnorm()
x_1<-rnorm(500)
x_1<-as.data.frame(x_1)
ggplot(x_1, aes(x = x_1)) + geom_density()


apply(wine,2,function(x) shapiro.test(scale(x)))

install.packages('MVN')
library(MVN)
mvtest<-mvn(wine[,10:11])
qqplot(mvtest)
