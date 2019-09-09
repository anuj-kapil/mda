xgetwd()
library(data.table)
cities <- fread('Data/cities15000AU.csv')
cities2 <- cities[,list(latitude,longitude,population)]
#subset(cities,select=c("latitude","longitude","population"))
setDF(cities2)
rownames(cities2)<-cities$name


head(cities2,4)

print(cities2, row.names = T)
cities2[100:109,1:2]

round(dist(cities2[100:109,1:2],method = "maximum"))
?dist

round(dist(cities2[100:109,],method = "manhattan"))

round(dist(cities2[100:109,],method = "minkowski", p=1))

round(dist(cities2[100:109,],method = "euclidean"))

round(dist(cities2[100:109,],method = "minkowski", p=2))

round(dist(cities2[100:109,],method = "minkowski", p=3))
round(dist(cities2[100:109,],method = "minkowski", p=4))

scaledcities<-scale(cities2)


round(dist(scaledcities[100:109,],method = "maximum"))
dist(scaledcities[100:109,],method = "minkowski",p=1)


#MVN
library(mnormt)

mv<-c(1,2)
vcmat<-matrix(c(1,0,0,1),nrow=2)
x <- seq(from=mv[1]-4*vcmat[1,1],to=mv[1]+4*vcmat[1,1],by=vcmat[1,1]/25)
y <- seq(from=mv[2]-4*vcmat[2,2],to=mv[2]+4*vcmat[2,2],by=vcmat[2,2]/25)
z <- matrix(0,201,201)
for(i in 1:201){
  for(j in 1:201){
    z[i,j]<-dmnorm(c(x[i],y[j]), mean = mv, vcmat, log = FALSE)
  }
}
dim(z)
persp(x,y,z, axes = TRUE,box = TRUE)
?persp
contour(x,y,z, axes = TRUE)

x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)



