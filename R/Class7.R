library(car)
getwd()
notes_data<-read.csv("Data/Notes.csv")
notes_data$Group<- substr(notes_data$Status,1,1) 

scatterplotMatrix(~Length+Height+Height.1+Inner.Frame+Inner.Frame.1+Diagonal | Group,data=notes_data)


cov(notes_data[,1:6])
notes_pc<-princomp(notes_data[,1:6])
summary(notes_pc, loadings = TRUE)


screeplot(notes_pc, npcs = 7, type = "lines")


biplot(notes_pc,xlabs=notes_data[,8],cex=c(0.8,1.4),
       expand=0.9,col=c("lightblue","black"))


notes_pc_sc<-princomp(notes_data[,1:6],cor=TRUE)
cor(notes_data[,1:6])



summary(notes_pc_sc, loadings = TRUE)


screeplot(notes_pc_sc, npcs = 7, type = "lines")



biplot(notes_pc_sc,xlabs=notes_data[,8],cex=c(0.8,1.4),
       expand=0.9,col=c("lightblue","black"))
