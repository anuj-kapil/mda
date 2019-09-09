Length<-c(190.5, 172.5, 167., 169.5, 175., 177.5, 179.5, 179.5, 173.5,
          162.5, 178.5, 171.5, 180.5, 183., 169.5, 172., 170., 182.5,
          179.5, 191., 184.5, 181., 173.5, 188.5, 175., 196., 200., 185.,
          174.5, 195.5, 197., 182.5)
Breadth<- c(152.5, 132., 130., 150.5, 138.5, 142.5, 142.5, 138., 135.5,
            139.,135., 148.5, 139., 149., 130., 140., 126.5, 136., 135.,
            140.5, 141.5, 142., 136.5, 130., 153., 142.5, 139.5, 134.5,
            143.5, 144., 131.5, 131.)
Height <- c(145., 125.5, 125.5, 133.5, 126., 142.5, 127.5, 133.5, 130.5,
            131., 136., 132.5, 132., 121.5, 131., 136., 134.5, 138.5,
            128.5, 140.5, 134.5, 132.5, 126., 143., 130., 123.5, 143.5,
            140., 132.5, 138.5, 135., 135.)
Fheight <- c(73.5, 63., 69.5, 64.5, 77.5, 71.5, 70.5, 73.5, 70., 62., 71.,
             65., 74.5, 76.5, 68., 70.5, 66., 76., 74., 72.5, 76.5, 79.,
             71.5, 79.5, 76.5, 76., 82.5, 81.5, 74., 78.5, 80.5, 68.5)
Fbreadth <- c(136.5, 121., 119.5, 128., 135.5, 131., 134.5, 132.5, 133.5,
              126., 124., 146.5, 134.5, 142., 119., 133.5, 118.5, 134.,
              132., 131.5, 141.5, 136.5, 136.5, 136., 142., 134., 146.,
              137., 136.5, 144., 139., 136.)
Type<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
Tibet <- data.frame(Length,Breadth,Height,Fheight,Fbreadth,Type)


skulltest<-summary(manova(cbind(Length, Breadth, Height, Fheight,
                                Fbreadth) ~ Type),test="Hotelling")
skulltest


install.packages('ICSNP')
library(ICSNP)
skulltest2 <- HotellingsT2(cbind(Length, Breadth, Height, Fheight, Fbreadth) ~
                             Type)
skulltest2


skulls1<-Tibet[Tibet$Type==1,1:5]
skulltest3<-HotellingsT2(skulls1[1:5], mu=c(170,150,140,80,130))
skulltest3

head(skulls1,1)

mean(skulls1[1])


library(car)
scatterplotMatrix(~ Length+Breadth+Height+Fheight+Fbreadth | Type,
                  data=Tibet, smooth=FALSE, reg.line=FALSE, ellipse=TRUE, by.groups=TRUE,
                  diagonal="none")

?scatterplotMatrix


scatterplotMatrix(~ Length+Breadth+Height+Fheight+Fbreadth, data=skulls1,
                  smooth=FALSE, reg.line=FALSE, ellipse=TRUE, by.groups=FALSE,
                  diagonal="none")



Xbar <- colMeans(skulls1)
S <- cov(skulls1)
n <- nrow(skulls1)
p <- ncol(skulls1)
for (i in 1:5) {
  lower <- round(Xbar[i] - sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  upper <- round(Xbar[i] + sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  cat(paste("The 95% CI for ", colnames(skulls1)[i], " is: (", lower, ", ",
            upper, ")\n"))
}


#univariate analysis
for (i in 1:5) {
  ci <- round(t.test(skulls1[, i])$conf.int, 3)
  cat(paste("The 95% CI for ", colnames(skulls1)[i], " is: (", ci[1], ", ",
            ci[2], ")\n"))
}


#Bonferroni

for (i in 1:5) {
  ci <- round(t.test(skulls1[, i], conf.level = (1 - 0.05/p))$conf.int, 3)
  cat(paste("The 95% Bonferroni CI for ", colnames(skulls1)[i], " is: (",
            ci[1], ", ", ci[2], ")\n"))
}
