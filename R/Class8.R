getwd()


notes_data<-read.csv("Data/Notes_8.csv")
notes_data$Group<-substr(notes_data$Status,1,1)

cov(notes_data[,1:6])
sapply(1:3, function(nf) factanal(notes_data[,1:6], factors = nf,method = "mle")$PVAL)
factanal(x=notes_data[,1:6], factors = 3, method = "mle")

principal(notes_data[,1:6],nfactors=3,rotate="none")



activities <- read.csv("C:/Documents/Activities.csv")
activities_pc<-princomp(activities,scale=TRUE)
cor(activities)


sapply(2, function(nf) factanal(activities, factors = nf,method = "mle")$PVAL)
factanal(x=activities, factors = 3, method = "mle")
covarmat=cov(activities)
factanal(covmat=covarmat, factors = 3, method = "mle")
## So MLE doesn't work!

principal(activities,nfactors=6,rotate="none")
