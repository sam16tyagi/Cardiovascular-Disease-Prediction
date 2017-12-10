#Bigmart Data Description
colnames(new_train)
#1. Item_Identifier-Unique product ID
#2. Item_Weight-Weight of product
#3. Item_Fat_Content-Whether the product is low fat or not
#4. Item_Visibility-The % of total display area of all products in a store allocated to the particular product
#5. Item_Type-The category to which the product belongs
#6. Item_MRP-Maximum Retail Price (list price) of the product
#7. Outlet_Identifier-Unique store ID
#8. Outlet_Establishment_Year-The year in which store was established
#9. Outlet_Size-The size of the store in terms of ground area covered
#10. Outlet_Location_Type-The type of city in which the store is located
#11. Outlet_Type-Whether the outlet is just a grocery store or some sort of supermarket
#12. Item_Outlet_Sales-Sales of the product in the particulat store. This is the outcome variable to be predicted.

#Import dataset
bigmart_train<-train
bigmart_test<-Test

str(bigmart_train)
str(bigmart_test)
bigmart_test$Item_Outlet_Sales<-0
comb_bigmart<-rbind(bigmart_train,bigmart_test)
dim(comb_bigmart)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(mice)
library(rpart)
library(randomForest)
library(missForest)
library(corrplot)
#making category names uniform
table(comb_bigmart$Item_Fat_Content)
levels(comb_bigmart$Item_Fat_Content)[1:3]<-"Low Fat"
levels(comb_bigmart$Item_Fat_Content)[2:3]<-"Regular"
 
table(comb_bigmart$Item_Type)
#Fat Levels make sense only for food Items
levels(comb_bigmart$Item_Fat_Content)<-c(levels(comb_bigmart$Item_Fat_Content),"None")
comb_bigmart[which(comb_bigmart$Item_Type%in%c("Health and Hygiene","Household","Others")) ,]$Item_Fat_Content<-"None"

table(comb_bigmart$Outlet_Size) #a good number of the outlets have not been classified based on the size
levels(comb_bigmart$Outlet_Size)[1]<-"uk"
prop.table(table(comb_bigmart$Outlet_Size,comb_bigmart$Outlet_Type),2)
plot(comb_bigmart$Outlet_Type,comb_bigmart$Outlet_Size)
levels(comb_bigmart$Outlet_Size)[1]<-'Small'

table(comb_bigmart$Outlet_Location_Type) #good to go

table(comb_bigmart$Outlet_Type)



table(comb_bigmart$Outlet_Identifier)# good to go

comb_bigmart$Years<-as.factor(2013-comb_bigmart$Outlet_Establishment_Year)

table(comb_bigmart$Outlet_Location_Type)
numerics<-names(comb_bigmart[,c(2,4,6)])
numerics<-comb_bigmart[,numerics]
colSums(is.na(numerics))



#2439 values of weights are missing
ggplot(data = comb_bigmart,aes(Item_Type,Item_Weight))+
  geom_boxplot()+ggtitle("Item Weight by Item type")+theme(axis.text = element_text(angle =90,vjust = 0.5,colour = "BLUE"))

ggplot(data = comb_bigmart,aes(Outlet_Identifier,Item_Weight))+
  geom_boxplot()+ggtitle("Item Weight by Item type")+theme(axis.text = element_text(angle =90,vjust = 0.5,colour = "BLUE"))
#Missing Values are from the two outlets OUT19 and OUT27

Missing_weights<-subset(x=comb_bigmart,Outlet_Identifier %in% c("OUT019","OUT027"),select = c("Outlet_Identifier","Item_Weight","Item_Type","Item_MRP"))

library(caret)
?rpart

#Missing Values Imputation by classification
#Weight_fit<-rpart(Item_Weight~Item_Identifier,data=comb_bigmart[!is.na(comb_bigmart$Item_Weight),],method='class')
#comb_bigmart$Item_Weight[is.na(comb_bigmart$Item_Weight)]<-predict(Weight_fit,comb_bigmart[is.na(comb_bigmart$Item_Weight),])

WeightsByItem<-ddply(na.omit(comb_bigmart),~Item_Identifier,summarise,mean=mean(Item_Weight),sdev=sd(Item_Weight))

comb_bigmart$Item_Weight<-ifelse(is.na(comb_bigmart$Item_Weight),WeightsByItem$mean[match(comb_bigmart$Item_Identifier,WeightsByItem$Item_Identifier)],comb_bigmart$Item_Weight)

str(comb_bigmart)


summary(comb_bigmart$Item_MRP)
plot(density(comb_bigmart$Item_MRP,kernel = 'gaussian'))
hist(comb_bigmart$Item_MRP,breaks=4)
abline(v=210)#70,135,210
comb_bigmart$Pricing_Category<-ifelse(comb_bigmart$Item_MRP<70,"Low",
                                      ifelse(comb_bigmart$Item_MRP<135,"Medium",
                                             ifelse(comb_bigmart$Item_MRP<210,"High","Very High"))) 
    
comb_bigmart$Pricing_Category<-as.factor(comb_bigmart$Pricing_Category)
histogram(comb_bigmart$Item_Visibility) # A lot of zero's
#Item_visibility being zero is quite fishy
iv<-subset(x=comb_bigmart,Item_Visibility==0,select = c('Item_Identifier','Outlet_Identifier','Outlet_Location_Type','Item_Type','Outlet_Type','Item_MRP','Item_Visibility','Outlet_Size'))
summary(iv)
 #The summary shows us that factors like Item_type,Outlet_Type,Outlet_Size and Outlet_Location_Type 
#could be having a major impact on Item_Visibility... hence visualizing them could be helpful

ggplot(data=comb_bigmart,aes(y=Item_Visibility,x=Item_Type, fill= Outlet_Type))+
         geom_boxplot()+
         ggtitle("Item Visibility vs Item Type by Outlet types")

#Item Visibility outlets wise
ggplot(data = comb_bigmart,aes(x=Outlet_Identifier,y=Item_Visibility,fill=factor(Outlet_Identifier)))+geom_boxplot()+ggtitle("Item Visibility by Outlets")

nonzeroiv<-subset(x=comb_bigmart,Item_Visibility>0)

comb_bigmart[which(comb_bigmart$Item_Visibility==0),]$Item_Visibility<-NA
colSums(is.na(comb_bigmart))
ivfit<-mice(comb_bigmart,m=1,maxit=1,method = "pmm",seed=0)
comb_bigmart<-complete(ivfit,1)

aggregate(comb_bigmart$Item_Visibility,by=list(comb_bigmart$Outlet_Identifier),FUN=sum)

Visibility_by_Outlet<-aggregate(comb_bigmart$Item_Visibility,by=list(comb_bigmart$Outlet_Identifier),FUN=sum)

Visibility_by_Outlet
numerics<-names(comb_bigmart[,c(2,4,6,12)])
numerics<-comb_bigmart[,numerics]
num_cor<-cor(numerics[1:nrow(bigmart_train),])
corrplot(num_cor,method="number",type="upper",order = "hclust")
#Model-Building
#rpart

new_mart<-comb_bigmart[,-c(1,13)]
new_train<-new_mart[1:nrow(bigmart_train),]
new_test<-new_mart[-(1:nrow(bigmart_train)),]
regpredict<-rpart(Item_Outlet_Sales~.,data=new_train,method='anova',control = rpart.control(cp=0.01,xval=10))
pred<-predict(regpredict,new_test)
colnames(new_train)
#Random forest

kernlab::predict()
library(caret)
ctrl<-trainControl(method="cv",number=10)
cart_fit<-train(x=new_train[,-11],y=new_train[,11],trControl=ctrl,method="ranger")
pred<-kernlab::predict(cart_fit,new_test)
#Writing to a submission file
sample <- read.csv("SampleSubmission_T.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = pred)
write.csv(pred, "sub1.csv",row.names = F)


