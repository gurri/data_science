getwd()
setwd("//Users/gurri/Downloads/ISLR_HW1")
df=read.csv('CANCER.csv')
summary(df)

####remove NAs

median_1<-median(df$PctEmployed16_Over,na.rm = TRUE)
df$PctEmployed16_Over[is.na(df$PctEmployed16_Over)]<-median_1

any(is.na(df$PctEmployed16_Over))

median_2<-median(df$PctPrivateCoverageAlone,na.rm=TRUE)
df$PctPrivateCoverageAlone[is.na(df$PctPrivateCoverageAlone)]<-median_2

any(is.na(df$PctPrivateCoverageAlone))
colnames(df)



summary(df)
data=df


summary(data)
data=data[data$MedianAge<=70, ]  

cor(data$avgAnnCount,data$avgDeathsPerYear)
#removing avg annualcount
data=data[,-1]
data=data[,-15]
#remove outliers
#summary(dum)

which(is.na(data))
data=data[-boxplot.stats(data$avgDeathsPerYear)$out,]

data=data[-boxplot.stats(data$incidenceRate)$out, ]
#data=data[-boxplot.stats(data$popEst2015)$out, ]
data=data[data$studyPerCap<=500, ] 
#data=data[-boxplot.stats(data$studyPerCap)$out, ]

#dum=dum[-boxplot.stats(dum$MedianAge)$out, ]

#dum=dum[-boxplot.stats(dum$MedianAge)$out, ]
#str(dum)
colnames(dum)
data=data[,-8]
data=data[,-11]

cor(data$TARGET_deathRate,data)

library(ggcorrplot)
corr<-cor(data)
ggcorrplot(corr)


library(caret)
#sample data, 4 numeric fields and 2 categorical
#df <- data.frame(a=runif(10), b=runif(10), c=1:10, d=1:10, e=letters[1:10], f=letters[1:10])

#categorical columns
#cat_cols <- c('e', 'f')
#remove categorical
#df2 <- data[!names(data) %in% cat_cols]

#run correlations


#the index of the columns to be removed because they have a high correlation
index <- findCorrelation(abs(corr), 0.75,exact=FALSE)

#the name of the columns chosen above
to_be_removed <- colnames(corr)[index]

#now go back to df and use to_be_removed to subset the original df
data=data[!names(data) %in% to_be_removed]
#remove employ over 16
data=data[,-14]
#remove no HS 18-24
data=data[,-9]
summary(data)


smp_size <- floor(0.80 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train_df <- data[train_ind, ]
test_df <- data[-train_ind, ]
#colnames(dum)
#########################Backward selection#################


set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(TARGET_deathRate ~., data = train_df,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:19),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel,9)

#####modelling on 9 features################

#data=data[ , which(names(dum) %in% c("avgDeathsPerYear","popEst2015","MedianAgeMale","MedianAgeFemale","AvgHouseholdSize","PercentMarried",
   #                                 "PctNoHS18_24","PctHS18_24","PctBachDeg18_24","PctHS25_Over","PctBachDeg25_Over",
  #                                  "PctUnemployed16_Over","PctPrivateCoverageAlone","PctEmpPrivCoverage","PctPublicCoverageAlone","PctWhite","PctBlack",
 #                                   "BirthRate"))]
#dum=dum[,-2]


#smp_size <- floor(0.75 * nrow(dum))



linearMod <- lm(TARGET_deathRate ~ studyPerCap+incidenceRate+PctBachDeg25_Over+
                  MedianAgeMale+PctHS18_24+PctUnemployed16_Over+PctOtherRace+PctMarriedHouseholds
                  +BirthRate, data=train_df) 



# build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)

deathrate_pred <- predict(linearMod, test_df)

actuals_preds <- data.frame(cbind(actuals=test_df$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 91.71%, min_max accuracy--------
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 9.36%, mean absolute percentage deviation

#rmse---21.18----------20.03
sqrt(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))

install.packages("metrics")
liinstallbrary(metrics)

#mse(actuals_preds$actuals,actuals_preds$predicteds)








#any(is.na(data))
#any(is.na(df$avgAnnCount))
#log(df$avgAnnCount)

#boxplot.stats((df$MedianAge))$out
#
#df$MedianAge[df$MedianAge>=70]
#hist(df$)
colnames(df)
df=df[,-18]

colnames(df)
summary(df)
str(df)
#removed nas
library(reshape2)
library(ggplot2)
d <- melt(df)
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()


#remove outliers
summary(dum)
dum=df

dum=dum[-boxplot.stats(dum$avgAnnCount)$out, ]

dum=dum[-boxplot.stats(dum$avgDeathsPerYear)$out, ]

dum=dum[-boxplot.stats(dum$incidenceRate)$out, ]
dum=dum[-boxplot.stats(dum$popEst2015)$out, ]
dum=dum[-boxplot.stats(dum$studyPerCap)$out, ]

dum=dum[-boxplot.stats(dum$MedianAge)$out, ]

#dum=dum[-boxplot.stats(dum$MedianAge)$out, ]
#str(dum)
colnames(dum)
dum=dum[,-9]
dum=dum[,-12]

cor(dum$TARGET_deathRate,dum)


colnames(dum)
dum=dum[ , -which(names(dum) %in% c("avgDeathsPerYear","popEst2015","MedianAgeMale","MedianAgeFemale","AvgHouseholdSize","PercentMarried",
                                "PctNoHS18_24","PctHS18_24","PctBachDeg18_24","PctHS25_Over","PctBachDeg25_Over",
                                "PctUnemployed16_Over","PctPrivateCoverageAlone","PctEmpPrivCoverage","PctPublicCoverageAlone","PctWhite","PctBlack",
                                "BirthRate"))]
#dum=dum[,-2]


smp_size <- floor(0.75 * nrow(dum))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dum)), size = smp_size)

train <- dum[train_ind, ]
test <- dum[-train_ind, ]
colnames(dum)

linearMod <- lm(TARGET_deathRate ~ ., data=train)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

deathrate_pred <- predict(linearMod, test)

actuals_preds <- data.frame(cbind(actuals=test$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 91.4%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 9.60%, mean absolute percentage deviation

#rmse---21.18
sqrt(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))

install.packages("metrics")
liinstallbrary(metrics)

mean((actuals_preds$actuals)^2 - (actuals_preds$predicteds)^2)
mse <- function(sm) 
  mean(sm$residuals^2)


install.packages("randomForest")
library(e1071)
model_reg = svm(train$TARGET_deathRate~., data=train)
print(model_reg)

pred = predict(model_reg, test)

x = 1:length(test$TARGET_deathRate)
plot(x, test$TARGET_deathRate, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

#plot(linearMod)
install.packages("caret")
library(caret)
sqrt(mean((test$TARGET_deathRate - pred)^2))
#20.96 
mse = MSE(test$TARGET_deathRate, pred)
mae = MAE(test$TARGET_deathRate, pred)
rmse = RMSE(test$TARGET_deathRate, pred)
r2 = R2(test$TARGET_deathRate, pred, form = "traditional")

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

dum1<-df



dum1=dum1[-boxplot.stats(dum1$avgAnnCount)$out, ]

#dum1=dum1[-boxplot.stats(dum1$avgDeathsPerYear)$out, ]

dum1=dum1[-boxplot.stats(dum1$incidenceRate)$out, ]
#dum1=dum1[-boxplot.stats(dum1$popEst2015)$out, ]
dum1=dum1[-boxplot.stats(dum1$studyPerCap)$out, ]

dum1=dum1[-boxplot.stats(dum1$MedianAge)$out, ]

#dum=dum[-boxplot.stats(dum$MedianAge)$out, ]
#str(dum)
colnames(dum1)
dum1=dum1[,-9]
dum1=dum1[,-12]


####################Iter 2



#colnames(dum)
dum1=dum1[ , -which(names(dum1) %in% c("avgDeathsPerYear","popEst2015","MedianAgeMale","MedianAgeFemale","AvgHouseholdSize","PercentMarried",
                                    "PctNoHS18_24","PctHS18_24","PctBachDeg18_24","PctHS25_Over","PctBachDeg25_Over",
                                    "PctUnemployed16_Over","PctPrivateCoverageAlone","PctEmpPrivCoverage","PctPublicCoverageAlone","PctWhite","PctBlack",
                                    "BirthRate"))]
#dum=dum[,-2]


smp_size <- floor(0.75 * nrow(dum1))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dum1)), size = smp_size)

train <- dum1[train_ind, ]
test <- dum1[-train_ind, ]
colnames(dum1)

linearMod <- lm(TARGET_deathRate ~ ., data=train)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)
deathrate_pred <- predict(linearMod, test)

actuals_preds <- data.frame(cbind(actuals=test$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 91.4%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 9.60%, mean absolute percentage deviation

#rmse---21.18
(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))

install.packages("Metrics")
library(Metrics)
#a=mse(actuals_preds$actuals,actuals_preds$predicteds)





pred = predict(model_reg, test)

x = 1:length(test$TARGET_deathRate)
plot(x, test$TARGET_deathRate, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

#plot(linearMod)
install.packages("caret")
library(caret)
sqrt(mean((test$TARGET_deathRate - pred)^2))
#20.96 
mse = MSE(test$TARGET_deathRate, pred)
mae = MAE(test$TARGET_deathRate, pred)
rmse = RMSE(test$TARGET_deathRate, pred)
r2 = R2(test$TARGET_deathRate, pred, form = "traditional")

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)


###########Forwrd sel

install.packages("FWDselect")
library(devtools)
install.packages('leaps')

devtools::install_github("rsquaredacademy/olsrr")
model <- lm(TARGET_deathRate ~ ., data = dum1)
ols_step_forward_p(model)

install.packages("caret")
install.packages("tibble")
install.packages('ggplot2')
library(ggplot2)
library(caret)
install.packages("caret",
                 repos = "http://cran.r-project.org" 
                 )

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('tibble', dependencies = TRUE)


set.seed(123)
train_ind <- sample(seq_len(nrow(dum1)), size = smp_size)

train <- dum1[train_ind, ]
test <- dum1[-train_ind, ]


set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(TARGET_deathRate ~., data = train,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:31),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 10)
########apply 10 features on data now

train1=train[,c('incidenceRate','MedianAgeMale','PercentMarried','PctHS18_24','PctBachDeg25_Over','PctEmployed16_Over','PctPrivateCoverage','PctEmpPrivCoverage',
         
         'PctOtherRace','PctMarriedHouseholds','TARGET_deathRate')]
test1=test[,c('incidenceRate','MedianAgeMale','PercentMarried','PctHS18_24','PctBachDeg25_Over','PctEmployed16_Over','PctPrivateCoverage','PctEmpPrivCoverage',
                
                'PctOtherRace','PctMarriedHouseholds','TARGET_deathRate')]



##apply model

linearMod <- lm(TARGET_deathRate ~ .-PercentMarried-PctEmpPrivCoverage, data=train1)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)
deathrate_pred <- predict(linearMod, test)

actuals_preds <- data.frame(cbind(actuals=test1$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 92.19%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 8.62%, mean absolute percentage deviation

#rmse---20.42
(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))

install.packages("Metrics")
library(Metrics)
rmse(actuals_preds$actuals,actuals_preds$predicteds)


install.packages("ggcorrplot")
library(ggcorrplot)
graph=train_df[ , which(names(train_df) %in% c("studyPerCap","incidenceRate", 
                                           "PctBachDeg25_Over" , "MedianAgeMale" , "PctHS18_24" , "PctUnemployed16_Over" , 
                                           "PctOtherRace" ,"PctMarriedHouseholds" , "BirthRate","TARGET_deathRate"))]

cormat<-cor(graph)
ggcorrplot(cormat)

plot(graph$incidenceRate,graph$TARGET_deathRate)


#remove emppriv coverage as mulicollinearity



##apply model

linearMod <- lm(TARGET_deathRate ~ ., data=train)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)
deathrate_pred <- predict(linearMod, t2)

actuals_preds <- data.frame(cbind(actuals=test1$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 92.19%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 8.62%, mean absolute percentage deviation

#rmse---20.42
(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))
rmse(actuals_preds$actuals,actuals_preds$predicteds)



####
t1=train1[,-8]
t1=t1[,-3]
cormat<-cor(t1)
ggcorrplot(cormat)


t1=t1[,-4]
t2=test1[,-3]
t2=t2[,-7]

#######remove more to prevent multicollinearity


linearMod <- lm(TARGET_deathRate ~ .,data=t1)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)
deathrate_pred <- predict(linearMod, test1)

actuals_preds <- data.frame(cbind(actuals=test1$TARGET_deathRate, predicteds=deathrate_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 92.19%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 8.62%, mean absolute percentage deviation

#rmse---20.42
(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))
rmse(actuals_preds$actuals,actuals_preds$predicteds)


###############################################


set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(TARGET_deathRate ~., data = df,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:11),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)



###splitgeography

library(stringr)
states=str_split_fixed(df$Geography, ",", 2)
states=as.data.frame(states)
colnames(states)<-c('county','state')

new_df<-cbind(df,states)

install.packages('sqldf')
library(sqldf)

a<-sqldf('select distinct state,avg(TARGET_deathRate) as avgdeathrate from new_df group by 1')

install.packages('tidyverse')
library(tidyverse)
install.packages('sf',dependencies = TRUE)
library(sf)
install.packages('maps')
install.packages('mapdata')
library(maps)
library(mapdata)

state <- map_data("state")

ggplot(data=state, aes(x=long, y=lat, fill=region, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

library(plotly)
install.packages('usmap')
library(usmap)
a<-as.data.frame(a)
typeof(df)
data.frame(unclass(summary(a)), check.names = FALSE, stringsAsFactors = FALSE)

write.csv(a,'new.csv')

plot_usmap(data = a, values = "avgdeathrate",regions="state")
+ 
  scale_fill_continuous(low = "white", high = "blue", name = "Avg Death Rates", label = scales::comma) + 
  labs(title = "Avg Cancer Death Rates ") +
  theme(legend.position = "right")


rpartImp <- varImp(linearMod)
