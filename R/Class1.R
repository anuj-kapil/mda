getwd()
install.packages('xlsx', dependencies = T)
install.packages('corrplot')
library(data.table)
library(xlsx)

library(corrplot)

boston <- fread('Data/boston.csv')


wine <- fread("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
colnames(wine) <- c("Cult", "Alc", "MalAcid", "Ash", "AshAlk", "Mag", "TotPhen",
                    "Flav", "NonFlav", "Proant", "Color", "Hue", "OD280OD315", "Proline")

summary(wine)

round(cor(wine), 2)
corrplot(round(cor(wine), 2))
pairs(wine[1:6])

library(scatterplot3d)

scatterplot3d(wine$Alc,wine$MalAcid,wine$Ash,highlight.3d=TRUE)

stars(wine, cex = 0.55)

install.packages('aplpack')
library(aplpack)

?stars



summary(boston)

round(cor(boston), 2)
corrplot(round(cor(boston), 2))


library(ggplot2)
library(gridExtra)


p1<-ggplot(boston, aes(x=boston$CRIM)) + geom_density()
p2<-ggplot(boston, aes(x=boston$ZN)) + geom_density()
p3<-ggplot(boston, aes(x=boston$INDUS)) + geom_density()
p4<-ggplot(boston, aes(x=boston$CHAS)) + geom_density()
p5<-ggplot(boston, aes(x=boston$NOX)) + geom_density()
p6<-ggplot(boston, aes(x=boston$RM)) + geom_density()
p7<-ggplot(boston, aes(x=boston$AGE)) + geom_density()
p8<-ggplot(boston, aes(x=boston$DIS)) + geom_density()
p9<-ggplot(boston, aes(x=boston$RAD)) + geom_density()
p10<-ggplot(boston, aes(x=boston$TAX)) + geom_density()
p11<-ggplot(boston, aes(x=boston$PTRATIO)) + geom_density()
p12<-ggplot(boston, aes(x=boston$B)) + geom_density()
p13<-ggplot(boston, aes(x=boston$LSTAT)) + geom_density()
p14<-ggplot(boston, aes(x=boston$MEDV)) + geom_density()
grid.arrange(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14, ncol=3)

##### Excercise

A<-matrix(c(2,-1,0,-1,2,-1,0,-1,2),nrow=3,byrow=TRUE)

# A inverse
A1<-solve(A)
# Floating point issue
round(A%*%A1)


options(scipen = 999)
B<-eigen(A)
B$values
B$vectors

P<-B$vectors
D<-B$values*diag(3)
Pt<-t(P)

round(P%*%D%*%Pt)

round(P%*%P1)

round(P1%*%P)

A
boston <- read.table("C:/Documents/boston.txt", sep = "") colnames(boston) <-
  c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS",
    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

#Load the whole data set

res <- readLines("http://users.stat.umn.edu/~kb/classes/5401/files/data/JWData5.txt")



#Figure out which lines we need. Each table reference will appear twice. Need to know the table you want and the next in the list

start<-grep("T01_06",res)[2]

end<-grep("T01_07",res)[2]



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

data.frame<-as.data.frame(datatable)

summary(data.frame)

data.frame[2:6]
scatterplot3d::scatterplot3d(data.frame[2:6])
install.packages('car')
library(car)
scatterplotMatrix(data.frame[2:6])
