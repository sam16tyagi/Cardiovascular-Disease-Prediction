# setting the directory and reading the data set
setwd("C:/Users/hp1/Desktop/R_random/pca")
# data extraction and preparation
dt<-read.csv("case_study_heart_disease_data_set (1) (1).csv",na.strings = "?")
View(dt)
dt<-na.omit(dt)
head(dt)
set.seed(111)
for(i in c(2,3,6,7,9,11,13,14)){
  dt[,i]<-as.factor(dt[,i])
}
summary(dt)
summary(dt$Age)
dt$Age<-cut(dt$Age,breaks = seq(28,78,10),labels = c("A","B","C","D","E"))
levels(dt$Age)
table(dt$Age)

summary(dt$chol)
dt$chol<-cut(dt$chol,breaks =3,labels=c("low","normal","high"))
levels(dt$chol)

summary(dt)
# data visualization

#Max.Heart rate achieved Vs Disease By Gender and Chestpain types"
ggplot(hd,aes(y=thalach,x=DV,fill=DV))+geom_boxplot()+facet_grid(Sex~cp,labeller = label_both)+ggtitle("   Max.Heart rate achieved Vs Disease By Gender and Chestpain types")+xlab("Presence of Heart Disease")+ylab("Max. Heart Rate Achieved")

#summary(hd$Sex)Cholestrol Levels Vs Disease By Gender and Chestpain types
ggplot(hd,aes(y=chol,x=DV,fill=DV))+geom_boxplot()+facet_grid(Sex~cp,labeller = label_both)+ggtitle("   Cholestrol Levels Vs Disease By Gender and Chestpain types")+xlab("Presence of Heart Disease")+ylab("Cholestrol Levels")

p<-ggplot(hd,aes(x=Age,y=thalach,color=DV))
# Max. Heart Rate Achieved Vs Age by DV
p+geom_point(size=3)+facet_grid(.~DV,labeller = label_both)+ylab("Max. Heart Rate Achieved")+theme_bw()+ggtitle("                       Max. Heart Rate Achieved Vs Age by DV")

p+geom_point(size=3)+facet_grid(thal~Sex+fbs,labeller = label_both)+ggtitle("Cholestrol Levels Vs Age By Gender and fasting blood sugar level")
q2

p1<-ggplot(hd,aes(x=trestbps,y=thalach,color=DV))
q3<-p1+geom_point()+facet_grid(.~DV)
q3

p1+geom_point(size=4)+facet_grid(thal~Sex+fbs,labeller=label_both)
q4

# splitting the data into training and test dataset
samp<-sample(2,nrow(dt),replace = T,prob = c(0.7,0.3))
training<-dt[samp==1,]
test<-dt[samp==2,]
head(training)
head(test)
colSums(is.na(test))
for(i in c(2,3,6,7,9,11,13,14)){
  training[,i]<-as.factor(training[,i])
}
for(i in c(2,3,6,7,9,11,13,14)){
  test[,i]<-as.factor(test[,i])
}
str(training)
str(test)
# data model
mod<-glm(DV~.,data = training,family = binomial('logit'))
summary(mod)
AIC(mod)
BIC(mod)
# prediction on training dataset
predicted<-predict(mod,training,type ="response")
training$predd<-round(predicted,3)
# cutoff value
View(training)
ggplot( training, aes( training$predd, color = as.factor(training$DV) ) ) + 
  geom_density( ) +
  ggtitle( "Training Set's Predicted Score" ) 
# using the cut off value to create a the prediction arrtibute 
training$predw<-ifelse(training$predd>0.46,1,0)
# checking model accuracy on training dataset
confusn<-confusionMatrix(training$DV,training$predw,threshold = 0.46)
confusn
class(confusn)
confusn<-as.matrix(confusn)
AccuracyRate <- sum(diag(confusn))/sum(confusn)
AccuracyRate
# predicting the results in prob for the validation dataset
test$prob<-predict(mod,test,type = "response")
View(test)

ggplot( test, aes( test$prob, color = as.factor(test$DV) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Test Set's Predicted Score" )

test$predc<-ifelse(test$prob>0.475,1,0)
# checking the model accuracy on validation dataset
confusn<-confusionMatrix(test$DV,test$predc,threshold = 0.475)
confusn
class(confusn)
confusn<-as.matrix(confusn)
AccuracyRate <- sum(diag(confusn))/sum(confusn)
AccuracyRate

test$DV <- as.factor(test$DV)
rocCurve   <- roc(response = test$DV, predictor = test$predc ,levels = rev(levels(test$DV)))
rocCurve
plot.roc(test$DV,test$predc)


predclass <-ifelse(test$predc>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = test$DV)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)

predclass
Confusion
AccuracyRate


plot(rocCurve)
rocCurve$sensitivities
rocCurve$specificities
rocCurve$au


# libraries involved
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(zoo)
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(boot) 
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(zoo)
library(InformationValue)
library(ROCR)
library(pROC)
