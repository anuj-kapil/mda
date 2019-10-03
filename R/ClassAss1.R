getwd()
library(data.table)
#Load the whole data set

res <- readLines("http://users.stat.umn.edu/~kb/classes/5401/files/data/JWData5.txt")

#Figure out which lines we need. Each table reference will appear twice. Need to know the table you want and the next in the list

start<-grep("T06_12",res)[2]

end<-grep("T06_13",res)[2]

rawtable<-res[start:(end-2)]

# Use the first line to find the dimensions of the table

infovec<-strsplit(rawtable[1]," ")[[1]]

infovec<-infovec[infovec!=""]

length<-as.numeric(infovec[2])

# Extract the rows containing the data (the last few rows)

start<-length(rawtable)-length+1

rawdata<-rawtable[start:length(rawtable)]

#Split the row into each data point

final_data<-strsplit(rawdata," ")

#Organise into a matrix, removing blank entries

datatable<-matrix(0,length,as.numeric(infovec[3]))

for(i in 1:length){
  
  row<-final_data[[i]]
  
  datatable[i,]<-as.numeric(row[row!=""])
  
} 

#Arrange in a data frame

data.frame <- as.data.frame(datatable)

colnames(data.frame) <- c('Gender','x1','x2','x3','x4')

male <- 1
female <- 2

setDT(data.frame)
obs.female <- data.frame[Gender==2]

library(ggplot2)
library(gridExtra)

# Question 1a
p1<-ggplot(obs.female, aes(x=obs.female$x2)) + geom_density()
p2<-ggplot(obs.female, aes(x=obs.female$x4)) + geom_density()
p1
p2
grid.arrange(p1, p2, ncol=2)


obs.female[,list(x1)]

par(mfrow = c(2, 1))
setDF(obs.female)
qqnorm(obs.female[,3], sub = colnames(obs.female)[3])
qqnorm(obs.female[,5], sub = colnames(obs.female)[5])
?qqnorm


# Question 1b

library(MVN)
mvtest<-roystonTest(wine[,10:11],qqplot=TRUE)
?mvn()
setDT(obs.female)
obs.female[,list(x2,x4)]

par(mfrow = c(1, 1))
mvn(obs.female[,list(x2,x4)], mvnTest='royston', multivariatePlot='qq')

# Question 1c
#
#One approach to dealing with data that do not follow a multivariate normal distribution
#is to perform a transformation. In practice, data are often transformed using a log transformation or a square root transformation to stabilise the variances of the response in
#each of the treatment groups when they differ significantly. These two transformations
#are in fact special cases of the Box–Cox transformation.
#

# Question 1d

library(car)
trans<-powerTransform(obs.female[,list(x2,x4)])
obs.female.sub<-obs.female[,list(x2,x4)]
obs.female.sub<-bcPower(obs.female.sub,trans$lambda)

par(mfrow = c(1, 1))
mvn(obs.female.sub, mvnTest='royston', multivariatePlot='qq')

# Question 1e

par(mfrow = c(2, 1))
setDF(obs.female.sub)
qqnorm(obs.female.sub[,1], sub = colnames(obs.female.sub)[1])
qqnorm(obs.female.sub[,2], sub = colnames(obs.female.sub)[2])



# Question 2a

concinna <- fread('Data/Concinna.csv')
concinna[, Species:=NULL]

setDF(concinna)

for (i in 1:2) {
  ci <- round(t.test(concinna[, i])$conf.int, 3)
  cat(paste("The 95% CI for ", colnames(concinna)[i], " is: (", ci[1], ", ",
            ci[2], ")\n"))
}

scatterplotMatrix(~ Width+Angle,
                  data=concinna, smooth=FALSE,  ellipse=TRUE, by.groups=TRUE,
                  diagonal="none")



# Question 2b
# u1 or Width is outside the 95% CI, so possibly not a concinna species

# Question 2c

Xbar <- colMeans(concinna)
S <- cov(concinna)
n <- nrow(concinna)
p <- ncol(concinna)
for (i in 1:2) {
  lower <- round(Xbar[i] - sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  5
  upper <- round(Xbar[i] + sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  cat(paste("The 95% CI for ", colnames(concinna)[i], " is: (", lower, ", ",
            upper, ")\n"))
}

# Question 2d
for (i in 1:2) {
  ci <- round(t.test(concinna[, i], conf.level = (1 - 0.05/p))$conf.int, 3)
  cat(paste("The 95% Bonferroni CI for ", colnames(concinna)[i], " is: (",
            ci[1], ", ", ci[2], ")\n"))
}

# Question 2e

###
### Write commentary

# Question 2f

par(mfrow = c(2, 1))
mvn(concinna, mvnTest='royston', multivariatePlot='qq')

p1<-ggplot(concinna, aes(x=concinna$Width)) + geom_density()
p2<-ggplot(concinna, aes(x=concinna$Angle)) + geom_density()
p1
p2
grid.arrange(p1, p2, ncol=2)

# Question 3a

##### Cov matrix

vcmat <- 1/5630 * matrix(c(575,-60,10,-60,300,-50,10,-50,196),nrow=3,byrow=TRUE)

# inverse
vcmat.inv <-solve(vcmat)

# Question 3b

# Question 3c
#MVN
library(MASS)
library(mnormt)

mv<-rep(0, 3)

mnd <- mvrnorm(n=10000,mv,vcmat)

# Question 3d

#Pair 1
x <- seq(from=mv[1]-4*vcmat[1,1],to=mv[1]+4*vcmat[1,1],by=vcmat[1,1]/25)
y <- seq(from=mv[2]-4*vcmat[2,2],to=mv[2]+4*vcmat[2,2],by=vcmat[2,2]/25)
length(x)
vcmat[1:2,1:2]
mv[1:2]
z <- matrix(0,201,201)
for(i in 1:201){
  for(j in 1:201){
    z[i,j]<-dmnorm(c(x[i],y[j]), mean = mv[1:2], vcmat[1:2,1:2], log = FALSE)
  }
}
?dmnorm
persp(x,y,z, axes = TRUE,box = TRUE)

contour(x,y,z, axes = TRUE)



?dmnorm()


# Question 3e
options(scipen = 999)
B<-eigen(vcmat)
B$values
B$vectors
# 
# P<-B$vectors
# D<-B$values*diag(3)
# Pt<-t(P)
# 
# round(P%*%D%*%Pt)
# 
# round(P%*%P1)
# 
# round(P1%*%P)
# 
# A

# Question 3f

# ??
cov(mnd)
# ??
vcmat

sample_pc<-princomp(mnd)
summary(sample_pc, loadings = TRUE)

par(mfrow = c(1, 1))
screeplot(sample_pc, npcs = 3, type = "lines")

# Question 3g

# Comp1 and Comp2 explains 82%

# Question 3h
mnd

mnd_df <- as.data.frame(as.table(mnd))
setDT(mnd_df)
mnd_dt <- dcast(mnd_df, Var1~Var2, value.var = 'Freq')
mnd_dt[,Var1:=NULL]
dim(mnd_dt)
summary(mnd_dt)
colnames(mnd_dt) <- c('Y1', 'Y2', 'Y3')

model<-lm(Y2~Y1, data = mnd_dt)

model_summary <- summary(model)
#Coefficent of Y1
model_summary$coefficients[[2]]

# Question 3i

model<-lm(Y3~Y1+Y2, data = mnd_dt)

model_summary <- summary(model)
#Coefficent of Y1
model_summary$coefficients[[2]]

#Coefficent of Y2
model_summary$coefficients[[3]]

# Question 3j
#Var(Y2|Y1)

# Question 3k
#Var(Y3|Y1, Y2)

#For (h)-(k), you should find two different ways (one way is via exact formulas, and the other
#way is via simulation in R) to compute the same answer. Then one way can be a check on the
#other.

# Question 4a

library(lars)
install.packages('lars')
data(diabetes)
x <- diabetes$x
x_df <- as.data.frame(as.table(x))
setDT(x_df)
x_dt <- dcast(x_df, Var1~Var2, value.var = 'Freq')
x_dt[,Var1:=NULL]
dim(x_dt)
summary(x_dt)




#Corr Plot
ncol(x_dt)
corr <- cor(x_dt, use = "pairwise.complete.obs")

library(ggcorrplot)
ggcorrplot(corr, hc.order = FALSE, type = "lower",
           ggtheme = ggthemes::theme_gdocs,
           colors = c("#ff7f0e", "white", "#1f83b4"),
           lab = TRUE)+
  theme(panel.grid.major=element_blank())


# Question 4b
x_dt[,y:=diabetes$y]

model<-lm(y~., data = x_dt)

model_summary <- summary(model)
model_summary$r.squared
anova <- anova(model)
sse <- anova$`Sum Sq`[[11]]
vif(model)


# Question 4c
model<-lm(y~age+bmi+map+hdl+ltg, data = x_dt)
model_summary <- summary(model)
model_summary$r.squared
#The idea is to reduce r square

anova(model)
vif(model)


# Question 4g

setDF(x_dt)

for (i in 1:10) {
  ci <- round(t.test(x_dt[, i])$conf.int, 3)
  cat(paste("The 95% CI for ", colnames(x_dt)[i], " is: (", ci[1], ", ",
            ci[2], ")\n"))
}

# Question 4h

setDF(x_dt)

Xbar <- colMeans(x_dt[1:10])
S <- cov(x_dt[1:10])
n <- nrow(x_dt[1:10])
p <- ncol(x_dt[1:10])
for (i in 1:10) {
  lower <- round(Xbar[i] - sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  5
  upper <- round(Xbar[i] + sqrt(p * (n - 1)/(n - p) * qf(0.95, p, n - p)) *
                   sqrt(S[i, i]/n), 3)
  cat(paste("The 95% CI for ", colnames(x_dt)[i], " is: (", lower, ", ",
            upper, ")\n"))
}
