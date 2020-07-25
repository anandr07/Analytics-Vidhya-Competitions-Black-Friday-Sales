library(Hmisc)
library(caret)
library(HH)
library(car)
library(ggplot2)
library(moments)
library(dplyr)
library(xgboost)


Rawdata=read.csv(file.choose())
View(Rawdata)
summary(Rawdata)
str(Rawdata)
describe(Rawdata)

traindata=Rawdata[,-c(1,2)]


traindata$Gender=as.factor(traindata$Gender)
traindata$Gender=as.numeric(traindata$Gender)
traindata$Age=as.factor(traindata$Age)
levels(traindata$Age)
traindata$Age=as.numeric(traindata$Age)
traindata$Occupation=as.factor(traindata$Occupation)
traindata$Occupation=as.numeric(traindata$Occupation)
traindata$City_Category=as.factor(traindata$City_Category)
traindata$City_Category=as.numeric(traindata$City_Category)
traindata$Stay_In_Current_City_Years=as.factor(traindata$Stay_In_Current_City_Years)
traindata$Stay_In_Current_City_Years=as.numeric(traindata$Stay_In_Current_City_Years)
traindata$Marital_Status=as.factor(traindata$Marital_Status)
traindata$Marital_Status=as.numeric(traindata$Marital_Status)
traindata$Product_Category_1=as.numeric(traindata$Product_Category_1)
mean(traindata$Product_Category_2)
mean(traindata$Product_Category_2,na.rm = TRUE)
median(traindata$Product_Category_2,na.rm = TRUE)
traindata$Product_Category_2[is.na(traindata$Product_Category_2)]=round(mean(traindata$Product_Category_2,na.rm = TRUE))
traindata$Product_Category_2=as.numeric(traindata$Product_Category_2)
traindata$Product_Category_3=as.numeric(traindata$Product_Category_3)
traindata$Purchase=as.numeric(traindata$Purchase)


#Test data
raw_test_data=read.csv(file.choose())
test_data=raw_test_data[,-c(1,2)]

str(test_data)
summary(test_data)
describe(test_data)

test_data$Gender=as.factor(test_data$Gender)
test_data$Gender=as.numeric(test_data$Gender)
test_data$Age=as.factor(test_data$Age)
levels(test_data$Age)
test_data$Age=as.numeric(test_data$Age)
test_data$Occupation=as.factor(test_data$Occupation)
test_data$Occupation=as.numeric(test_data$Occupation)
test_data$City_Category=as.factor(test_data$City_Category)
test_data$City_Category=as.numeric(test_data$City_Category)
test_data$Stay_In_Current_City_Years=as.factor(test_data$Stay_In_Current_City_Years)
test_data$Stay_In_Current_City_Years=as.numeric(test_data$Stay_In_Current_City_Years)
test_data$Marital_Status=as.factor(test_data$Marital_Status)
test_data$Marital_Status=as.numeric(test_data$Marital_Status)
test_data$Product_Category_1=as.numeric(test_data$Product_Category_1)
mean(test_data$Product_Category_2)
mean(test_data$Product_Category_2,na.rm = TRUE)
median(test_data$Product_Category_2,na.rm = TRUE)
test_data$Product_Category_2[is.na(test_data$Product_Category_2)]=median(test_data$Product_Category_2,na.rm = TRUE)
test_data$Product_Category_2=as.numeric(test_data$Product_Category_2)
test_data$Product_Category_3=as.numeric(test_data$Product_Category_3)


#Visualisation
attach(traindata)
ggplot(traindata,aes(x=Gender,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Age,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Occupation,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=City_Category,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Stay_In_Current_City_Years,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Marital_Status,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Product_Category_1,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Product_Category_2,fill=Purchase))+geom_bar(position = "dodge")+ylab("Purchase")

ggplot(traindata,aes(x=Purchase))+geom_density()
ggplot(traindata,aes(x=Product_Category_1))+geom_density()
ggplot(traindata,aes(x=Product_Category_2))+geom_density()
ggplot(traindata,aes(x=Purchase))+geom_histogram()

ggplot(traindata,aes(x=Product_Category_1))+geom_boxplot()
ggplot(traindata,aes(x=Product_Category_2))+geom_boxplot()


training=traindata[,-9]
testing=test_data[,-9]

labeltraining=as.numeric(training[[9]])
datatraining=as.matrix(training[1:8])

datatesting=as.matrix(testing[1:8])

xgtraining=xgb.DMatrix(data=datatraining,label=labeltraining)
xgtesting=xgb.DMatrix(data=datatesting)

parameter = list("objective" = "reg:linear", "bst:eta" = 0.005,"bst:max_depth" = 5,"nthread" = 4,"gamma" =0.5,"min_child_weight" = 3)

model = xgboost(params = parameter,nfold = 5, data = xgtraining, nround = 1200)

#predicting test data
purchase=predict(model,xgtesting)
purchase

sample_df=data.frame(purchase,raw_test_data$User_ID,raw_test_data$Product_ID)
sample_df=rename(sample_df,Purchase=purchase)
sample_df=rename(sample_df,User_ID=raw_test_data.User_ID)
sample_df=rename(sample_df,Product_ID=raw_test_data.Product_ID)

write.csv(sample_df,"C:\\Users\\priti\\Desktop\\Anand\\Analytics Vidhya\\Black friday sales\\SampleSubmission.csv",row.names=FALSE)
