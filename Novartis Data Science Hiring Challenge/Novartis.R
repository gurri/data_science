getwd()
setwd("C:/Users/oyo/Downloads/Novartis_Dataset/Dataset")

train<-read.csv("train.csv")
test<-read.csv("test.csv")


summary(train)

summary(test)


test[which(is.na(test$X_12)),14]<-1
#remove NAs
train<-train[-(which(is.na(train$X_12))),]

train$MULTIPLE_OFFENSE<-as.factor(as.character(train$MULTIPLE_OFFENSE))

library(GGally)

#Correlation between continous variables
cormatrix<-cor(train[,c(3:18)])
library(corrplot)
corrplot(cormatrix,method = "color")
any(is.na(train$X_12))



corrplot::corrplot(train[,c(3:18)])
library(plyr)
hist(count(train$MULTIPLE_OFFENSE))

hist(train$X_15)

boxplot(train$X_15)
boxplot.stats(train$X_15)

count(train$X_14<27)

#x1 ,x10,x12,x15 no use(all 1), dont take as all 0,x2 and x3 same so take x2,x4,X5,X6 ,x7,x8,x9 (<4)replace outliers by median,
#x11 repalce less than 67 by median,x13 repalce less than 27 by median,x14,



traincl<-train[,-c(1,2)]
boxplot.stats(traincl$X_9)

traincl$X_9[traincl$X_9<4]<-5

boxplot.stats(traincl$X_11)

traincl$X_11[traincl$X_11<67]<-249

boxplot.stats(traincl$X_13)

traincl$X_13[traincl$X_13<27]<-98


ggpair()


sample<-sample(c(1:nrow(train)))

##train on this
train1<-train[sample[1:16571],]

###test on this
train2<-train[sample[16572:nrow(train)],]

library(caret)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
head(subtrain)
subtrain<-train[,-c(1,2)]

summary(subtrain)

model_rf_under <- caret::train(MULTIPLE_OFFENSE ~ .,
                               data = subtrain,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_under <- data.frame(
                          predict(model_rf_under, newdata = test[,-c(1,2)], type = "prob"))
final_under$predict <- ifelse(final_under$X0 > 0.5,0,1)
final_under$predict<-as.factor(final_under$predict)

typeof(final_under)
final_under<-as.data.frame(final_under)
output<-cbind(test,final_under$predict)
outputrf<-output[,c(1,ncol(output))]
colnames(outputrf)<-c("INCIDENT_ID","MULTIPLE_OFFENSE")
outputrf<-as.data.frame(outputrf)
count(outputrf$MULTIPLE_OFFENSE)


write.csv(outputrf,"outnov.csv",row.names = FALSE)
cm_under <- confusionMatrix(final_under$predict, train2$MULTIPLE_OFFENSE)

#########oversample########

library(caretEnsemble)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 2, 
                     repeats = 2, 
                     verboseIter = TRUE,
                     sampling = "up",savePredictions = TRUE,classProbs = TRUE)

algolist=c('rf','gbm','svmRadial')

models <- caretList(make.names(MULTIPLE_OFFENSE) ~ .,
                              data = subtrain,
                              methodList = algolist,
                              preProcess = c("scale", "center"),
                              trControl = ctrl
)

stackControl<-trainControl(method = "repeatedcv", 
                           number = 2, 
                           repeats = 1, 
                           verboseIter = TRUE,
                           sampling = "up",savePredictions = TRUE,classProbs = TRUE)

stack.glm<-caretStack(models,method="glm",trControl=stackControl)


results<-resamples(models)

model_rf_over <- caret::train(MULTIPLE_OFFENSE ~ .,
                              data = subtrain,
                              method = "gbm",
                              preProcess = c("scale", "center"),
                              trControl = ctrl,metric='recall'
                             )


final_over1 <- data.frame(
  predict(stack.glm, newdata = test[,-c(1,2)]))
table(final_over1)
final_over1<-as.data.frame(final_over1)
final_over1[,1]<-as.character(final_over1[,1])

final_over1[,1][final_over1[,1]=='X0']<-0
final_over1[,1][final_over1[,1]=='X1']<-1

#final_over1$predict <- ifelse(final_over1[,1] > 0.5,0,1)
final_over1$predict.stack.glm..newdata...test....c.1..2...[final_over1$predict.stack.glm..newdata...test....c.1..2...=='X0']<-0
final_over1$predict<-as.factor(final_over1$predict)

typeof(final_over1)
final_over1<-as.data.frame(final_over1)
output2<-cbind(test,final_over1[,1] )
outputrfover<-output2[,c(1,ncol(output2))]
colnames(outputrfover)<-c("INCIDENT_ID","MULTIPLE_OFFENSE")
outputrfover<-as.data.frame(outputrfover)
count(outputrfover$MULTIPLE_OFFENSE)
write.csv(outputrfover,"out15.csv",row.names = F)






