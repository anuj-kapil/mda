install.packages('VGAM')
library(VGAM)
library(nnet)
library(data.table)
library(rpart)
library(glmnet)
library(MASS)
library(ggplot2)
library(klaR)


summary(data(MultinomialExample))

attach(MultinomialExample)
####### Part A

####### Q3

egyptskull <- fread('Data/egyptskull.csv')

summary(egyptskull)

egyptskull[, Epoch:= as.factor(Epoch)]
egyptskull[,.N, by = Epoch]

summary(egyptskull)

egyptskull[, unique(Epoch)]

ggplot(egyptskull, aes(x=MB, y=BH, group=Epoch))+
  goem_point(aes(color=Epoch))

# model_logit <- glm(Epoch ~ MB+BH+BL+NH, family = 'binomial', data = egyptskull)
# summary(model_logit)

egyptskull[, Epoch_1:=ifelse(Epoch == 4000, 1, 0)]
egyptskull[, Epoch_2:=ifelse(Epoch == 3300, 1, 0)]
egyptskull[, Epoch_3:=ifelse(Epoch == 1850, 1, 0)]
egyptskull[, Epoch_4:=ifelse(Epoch == 200, 1, 0)]
egyptskull[, Epoch_5:=ifelse(Epoch == 150, 1, 0)]


egyptskull_train <- egyptskull[,.SD[1:25], by = list(Epoch)]
egyptskull_test <- egyptskull[,.SD[26:30], by = list(Epoch)]

egyptskull_train[, .N, by = list(Epoch)]
egyptskull_test[, .N, by = list(Epoch)]

#######   LDA

model_lda <- lda(Epoch ~ MB+BH+BL+NH, data=egyptskull_train)
plot(model_lda)

egyptskull_test$lda_predict<-predict(model_lda,egyptskull_test[,2:5])$class
table(egyptskull_test$Epoch,egyptskull_test$lda_predict)

mean(egyptskull_test$lda_predict != egyptskull_test$Epoch)

partimat(as.factor(Epoch) ~ MB+BH+BL+NH, data=egyptskull_train,method="lda")

######## QDA

model_qda<-qda(Epoch ~ MB+BH+BL+NH, data=egyptskull_train)
model_qda

egyptskull_test$qda_predict<-predict(model_qda,egyptskull_test[,2:5])$class
table(egyptskull_test$Epoch,egyptskull_test$qda_predict)

mean(egyptskull_test$qda_predict != egyptskull_test$Epoch)

partimat(as.factor(Epoch) ~ MB+BH+BL+NH, data=egyptskull_train,method="qda")

######  Multinomial Logistic 

model_mnl<-vglm(formula = cbind(Epoch_1,Epoch_2,Epoch_3,Epoch_4,Epoch_5) ~ MB+BH+BL+NH, family = multinomial, data = egyptskull_train)
summary(model_mnl)

predictions<-predict(model_mnl,newdata=egyptskull_test[,2:5],type="response")
egyptskull_test$pred_mnl<-apply(predictions,1,function(i) which.max(i) )

egyptskull_test[, pred_mnl:= c(4000, 3300, 1850, 200, 150)[pred_mnl]]
egyptskull_test[, unique(Epoch)]
print(table(egyptskull_test$Epoch,egyptskull_test$pred_mnl))

mean(egyptskull_test$pred_mnl != egyptskull_test$Epoch)

######## CART

model_ct <- rpart(Epoch ~ MB+BH+BL+NH, data = egyptskull_train, method="class")
plot(model_ct)
text(model_ct, use.n=TRUE, all=TRUE, cex=.7)

plotcp(model_ct)

egyptskull_test$pred_ct<-predict(model_ct,egyptskull_test,type="vector")
egyptskull_test[, pred_ct:= c(4000, 3300, 1850, 200, 150)[pred_ct]]
table(egyptskull_test$Epoch,egyptskull_test$pred_ct)

mean(egyptskull_test$pred_ct != egyptskull_test$Epoch)

model_ct$cptable

model_ct_fit<- prune(model_ct, cp=model_ct$cptable[which.min(model_ct$cptable[,"xerror"]),"CP"])

summary(model_ct_fit)

plot(model_ct_fit)
text(model_ct_fit, use.n=TRUE, all=TRUE, cex=.7)

egyptskull_test$pred_ct_fit<-predict(model_ct_fit,egyptskull_test,type="vector")
egyptskull_test[, pred_ct_fit:= c(4000, 3300, 1850, 200, 150)[pred_ct_fit]]
table(egyptskull_test$Epoch,egyptskull_test$pred_ct_fit)

mean(egyptskull_test$pred_ct_fit != egyptskull_test$Epoch)

##### Nnet

model_nnet<-nnet(Epoch ~ MB+BH+BL+NH, data = egyptskull_train,size=5,decay=0.1)

egyptskull_test$pred_nnet<-predict(model_nnet,egyptskull_test,type="class")
table(egyptskull_test$Epoch,egyptskull_test$pred_nnet)

mean(egyptskull_test$pred_nnet != egyptskull_test$Epoch)



####### Predict
egyptskull_val <- data.table(rbind(c(128, 143, 103, 50) 
                                  , c(129, 126, 91, 50)
                                  , c(130, 127, 99, 45)
                                  , c(130, 131, 98, 53)
                                  , c(134, 124, 91, 55)
                                  , c(130, 130, 104, 49)
                                  , c(134, 139, 101, 49)
                                  , c(136, 133, 91, 49)
                                  ))

names(egyptskull_val) <- names(egyptskull)[1:4]

# Use multinomial

predictions<-predict(model_mnl,newdata=egyptskull_val,type="response")

egyptskull_val$pred_mnl<-apply(predictions,1,function(i) which.max(i) )

egyptskull_val[, pred_mnl:= c(4000, 3300, 1850, 200, 150)[pred_mnl]]

####### Part B

library(lars)
library(glmnet)

url <- 'https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data'
diabetes_orig <- fread(url, sep = '\t')

fwrite(diabetes_orig, 'Data/diabetes.csv')

# 
# data(diabetes)
# Xmatrix <- diabetes$x
# yVector <- diabetes$y
# 



dim(diabetes_orig)

Xmatrix <- as.matrix(diabetes_orig[,1:10])
yVector <- diabetes_orig$Y

dim(Xmatrix)

cvfit <- cv.glmnet(Xmatrix , yVector)

plot(cvfit)


log(cvfit$lambda.min)

coef(cvfit, s = "lambda.min")

log(cvfit$lambda.1se)
coef(cvfit, s = "lambda.1se")

LASSOfit <- glmnet(Xmatrix , yVector)
summary(LASSOfit)
LASSOfit$lambda

betaHat <- as.numeric(LASSOfit$beta)

LASSOfit <- glmnet(Xmatrix , yVector , lambda=2)
betaHat <- as.numeric(LASSOfit$beta)

LASSOfit <- glmnet(Xmatrix , yVector , lambda=1)
betaHat <- as.numeric(LASSOfit$beta)

?glmnet
