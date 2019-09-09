BOD_C<-c(6, 6, 18, 8, 11, 34, 28, 71, 43, 33, 20)
SS_C<-c(27, 23, 64, 44, 30, 75, 26, 124, 54, 30, 14)
BOD_S<-c(25, 28, 36, 35, 15, 44, 42, 54, 34, 29, 39)
SS_S<-c(15, 13, 22, 29, 31, 64, 30, 64, 56, 20, 21)
Comm<-cbind(BOD_C,SS_C)
State<-cbind(BOD_S,SS_S)
Diff<-Comm-State
colnames(Diff)<-c("BOD","SS")
library(car)
scatterplotMatrix(~ BOD+SS, data=Diff,
                  smooth=FALSE, reg.line=FALSE, ellipse=TRUE, by.groups=FALSE,
                  diagonal="none")


library(ICSNP)
wastetest<-HotellingsT2(Diff, mu=c(0,0))
wastetest
mean_D<-colMeans(Diff)
S_D<-cov(Diff)
p<-ncol(Diff)
n<-nrow(Diff)
for(i in 1:p){
  lower<-round(mean_D[i]-sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(S_D[i,i]/n),3)
  upper<-round(mean_D[i]+sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(S_D[i,i]/n),3)
  cat(paste("The 95% CI for ",colnames(Diff)[i]," is: (",lower,", ",upper,")\n"))
}

install.packages('heplots', dependencies = T)

library(biotools)
library(heplots)
library(car)
boxM(Tibet[,1:5],Tibet[,6])


Tibet_S<-Tibet[Tibet$Type==1,1:5]
Tibet_L<-Tibet[Tibet$Type==2,1:5]
Xbar_S<-colMeans(Tibet_S)
S_S<-cov(Tibet_S)
n_S<-nrow(Tibet_S)
p<-ncol(Tibet_S)
n<-ncol(Tibet_S)
Xbar_L<-colMeans(Tibet_L)
S_L<-cov(Tibet_L)
n_L<-nrow(Tibet_L)
d_bar<-Xbar_S-Xbar_L
Sd<-(n_S - 1)/(n_S + n_L - 2)*S_S + (n_L - 1)/(n_S + n_L - 2)*S_L
for(i in 1:p){
  lower<-round(d_bar[i]-sqrt(p*(n_S+n_L-2)/(n_S+n_L-p-1)*qf(0.95,p,n_S+n_L-p-1))
               *sqrt(Sd[i,i]*(1/n_S+1/n_L)),3)
  upper<-round(d_bar[i]+sqrt(p*(n_S+n_L-2)/(n_S+n_L-p-1)*qf(0.95,p,n_S+n_L-p-1))
               *sqrt(Sd[i,i]*(1/n_S+1/n_L)),3)
  cat(paste("The 95% CI for ",colnames(Tibet_L)[i]," is: (",lower,", ",upper,")\n"))
}