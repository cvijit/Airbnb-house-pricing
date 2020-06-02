#1- getting the data
data = read.csv("/Users/vijitchekkala/Desktop/berlin_airbnb/listings_summary.csv",header = T)
head(data)
tail(data)
str(data)

names(data)
#ignoring columns which have data with sentence
bnb <- data[c('id','host_has_profile_pic', 'neighbourhood_group_cleansed', 
              'property_type', 'room_type', 'accommodates', 'bathrooms',  
              'bedrooms', 'bed_type','square_feet', 'price', 'cleaning_fee', 
              'security_deposit', 'extra_people', 'guests_included', 'minimum_nights',  
              'instant_bookable', 'is_business_travel_ready', 'cancellation_policy')]
names(bnb)
head(bnb)
str(bnb)
#
library(stringr)
library(dplyr)
library(tidyr)
#bnb <- bnb$host_has_profile_pic %>% str_replace(bnb$host_has_profile_pic,"f","False") %>% str_replace(bnb$host_has_profile_pic,"t","True")

bnb$host_has_profile_pic <- str_replace(bnb$host_has_profile_pic,"f","False")
bnb$host_has_profile_pic <- str_replace(bnb$host_has_profile_pic,"t","True")
bnb$instant_bookable <- str_replace(bnb$instant_bookable,"f","False")
bnb$instant_bookable <- str_replace(bnb$instant_bookable,"t","True")
bnb$is_business_travel_ready <- str_replace(bnb$is_business_travel_ready,"f","False")
bnb$is_business_travel_ready <- str_replace(bnb$is_business_travel_ready,"t","True")
head(bnb)
#

library(readr)

bnb$price<- as.double(gsub('[$]', '', bnb$price))
bnb$cleaning_fee<- as.double(gsub('[$]', '', bnb$cleaning_fee))
bnb$security_deposit<- as.double(gsub('[$]', '', bnb$security_deposit))
bnb$extra_people<- as.double(gsub('[$]', '', bnb$extra_people))
sum(is.na(bnb))
#filling na values which happende because of corecion
bnb$price <- na.locf(na.locf(bnb$price), fromLast = FALSE)
bnb$cleaning_fee <- na.locf(na.locf(bnb$cleaning_fee), fromLast = FALSE)
bnb$security_deposit <- na.locf(na.locf(bnb$security_deposit), fromLast = FALSE)
sum(is.na(bnb))
str(bnb)
#conver 3 factor to char to int to boolean
bnb$host_has_profile_pic <- factor(0:1); str(bnb$host_has_profile_pic); as.logical(as.integer(bnb$host_has_profile_pic) - 1L)
bnb$instant_bookable <- factor(0:1); str(bnb$instant_bookable); as.logical(as.integer(bnb$instant_bookable) - 1L)
bnb$is_business_travel_ready <- factor(0:1); str(bnb$is_business_travel_ready); as.logical(as.integer(bnb$is_business_travel_ready) - 1L)

#
#plot missing values 
#install.packages('naniar')
library(naniar)
vis_miss(bnb)
#dropping square feet column becasue of more than 90 % missing data
bnb <- bnb[,-10]
#dropping id columns
bnb <- bnb[,-1]
summary(bnb)
vis_miss(bnb)

#libraries to use for missing data
library(mice)
library(VIM)
#checking with missing values percentage
na_values <- function(x) {sum(is.na(x)/length(x)*100)}
apply(bnb,2,na_values)
md.pattern(bnb)
#only bedroom and bathrooms now have a small amount of missing values
#bnb <- (bnb$bedrooms,.direction=c("down"))
#bnb(bnb$bathrooms,.direction=c("down"))
head(bnb)

library(zoo)
bnb$bedrooms <- na.locf(na.locf(bnb$bedrooms), fromLast = FALSE)
bnb$bathrooms  <- na.locf(na.locf(bnb$bathrooms), fromLast = TRUE)
#checking with missing values percentage
na_values <- function(x) {sum(is.na(x)/length(x)*100)}
apply(bnb,2,na_values)
#chekcing missing values
sum(is.na(bnb$bathrooms))
sum(is.na(bnb$bedrooms))
sum(is.na(bnb))
#
str(bnb)
# Simple Horizontal Bar Plot with Added Labels
count1 <- table(bnb$room_type)
barplot(count1, main="Room Type", horiz=FALSE,col=c("royalblue","navy","skyblue"),)
count2 <- table(bnb$property_type)
table(bnb$property_type)
head(bnb$property_type)
barplot(count2, main="Property Type", horiz=FALSE,col = 'red')

#selecting only numeric values
#bnb8086 <- numeric values
#bnb9905 <- facotr variables
#bnb001 <- combine
nums <- unlist(lapply(bnb, is.numeric)) 
head(nums)
bnb8086<- bnb[ , nums] 
head(bnb8086)# this vlaue for model
summary(bnb8086)
colnames(bnb8086)
#
factors <- unlist(lapply(bnb, is.factor))
bnb9905 <- bnb[,factors]
head(bnb9905)
colnames(bnb9905)

#hist(bnb8086) 
head(bnb8086$minimum_nights)

str(bnb8086)
#qplot(bnb$price,data = bnb8086, bins=50)
#feature engineering with one hot encoding
#install.packages('ade4')
library(ade4)
library(data.table)
ohe_feats = c("host_has_profile_pic","neighbourhood_group_cleansed","property_type",            
              "room_type","bed_type","instant_bookable",          
              "is_business_travel_ready","cancellation_policy" )
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(bnb9905[f])
  bnb9905[f] = NULL
  bnb9905 = cbind(bnb9905, df_all_dummy)
}
head( df_all_dummy)
head(bnb9905)
str(bnb8086)
###

bnb001 = cbind(bnb8086,bnb9905)
str(bnb001)
bnbknn <- bnb001
str(bnbknn)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
bnb80861 <- as.data.frame(lapply(bnb8086[,1:3], normalize))
head(bnb80861)
bnb80862 <- as.data.frame(lapply(bnb8086[,5:9], normalize))
head(bnb80862)
bnbfinal <- cbind(bnb80861,bnb80862)
head(bnbfinal)
head(bnbfinal1)
bnbfinal1 <- cbind(bnbfinal,bnb9905)
bnbfinal2 <- cbind(bnbfinal1,bnb$price)
head(bnbfinal2) # comibned aall values here
#rename bnb$rpice to price
bnbfinal2 %>% rename(bnb$price = price)
names(bnbfinal2)[names(bnbfinal2) == "bnb$price"] <- "price"
########
summary(bnb8086)
summary(bnbfinal1)
head(bnb8086)
summary(bnb8086$price)


library(caTools)
sample <- sample.split(bnb8086, SplitRatio = 0.80) #storing the train and test data in sample variable
training <- subset(bnb8086, sample==TRUE) #training data
test <-  subset(bnb8086, sample==FALSE) #testing data


#multiple regression training
model1 <- lm(price ~.,training)
summary(model1)

#predict
predict<- predict(model1,test)

results <- cbind(predict,test$price) 

head(results)
colnames(results) <- c('predicted','actual')
str(results)
summary(results)
results <- as.data.frame(results)
tail(results)
summary(results)
#mean square method
MSE <- mean((results$actual-results$predicted)^2)
print(MSE)

#root mean square method
MSE^0.5

#root squared value

SSE = sum((results$predicted - results$actual)^2)
SST = sum( (mean(bnb8086$price) - results$actual)^2)

R2 = 1 - SSE/SST
R2
#knn model
table(bnbknn$price)
round(prop.table(table(bnbknn$price)) * 100, digits = 1)
summary(bnbknn)
summary(bnb001)

#train or model  a data
train_labels <- bnbknn[1:17541, 1]
test_labels <- bnbknn[17542:22553, 1]
#buidling knn model
library(class)
prc_test_pred <- knn(train = training, test = test,cl = train_labels, k=10)
#evaluating
library(gmodels)#######3

#CrossTable(x=test_labels,y=prc_test_pred, prop.chisq = FALSE)
#
library(caret)
pred_caret <- train(training,train_labels,method = "knn", preProcess = c("center","scale"))
pred_caret
plot(pred_caret)
#
#install.packages('FNN')
library(FNN)
reg_results <- knn.reg(training, test, train_labels, k = 17)
print(reg_results)
plot(x=test_labels,y=reg_results$pred)

#mean square prediction error
mean((test_labels - reg_results$pred) ^ 2)

