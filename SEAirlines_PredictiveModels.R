## R SCRIPT - SOUTHEAST AIRLINES CUSTOMER SATISFACTION ANALYSIS ##

# Change to the folder containing your homework data files
setwd("~/Vicky/Fall19/Intro to Data Science/Homework")
#Adding jsonlite package to R library
library(jsonlite)
#Adding tidyverse package to R library.
library(tidyverse)
#Creating dataframe from JSON
df<-fromJSON("fall2019-survey-M03.json")

#Replacing Arrival delay with 0 where flight is not cancelled and no arrival delay
df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="No"]<-0

#create new dataframe where flight time was missing but flight was not cancelled
missingflighttime <- df%>%
  filter(is.na(df$Flight.time.in.minutes)&df$Flight.cancelled=="No")
for (var1 in unique(missingflighttime$Origin.City)){
  #Iterate through destination cities with missing flight times
  for (var2 in unique(missingflighttime$Destination.City)){
    #Calculate mean of know flight times between city pairs (origin to destination OR destination to origin) to replace NAs
    df$Flight.time.in.minutes[df$Origin.City==var1&df$Destination.City==var2&df$Flight.cancelled=="No"]<-mean(df$Flight.time.in.minutes[(df$Origin.City==var1&df$Destination.City==var2)|(df$Origin.City==var2&df$Destination.City==var1)], na.rm=TRUE)
  }
}

#Adding a new column for the type of customer
df$typeofCust <- cut(df$Likelihood.to.recommend, breaks=c(-Inf,6,8,Inf), labels=c("Detractors", "Passive", "Promoters"))
#Adding a new column as a dummy variable for type of customer
df$dummytypeCust <- cut(df$Likelihood.to.recommend, breaks=c(-Inf,6,Inf), labels=c("1", "0"))

#Replacing Departure.Delay.in.Minutes NA's with Mean Value.
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes))] <- round(mean(df$Departure.Delay.in.Minutes, na.rm=TRUE))
#Replacing Arrival.Delay.in.Minutes NA's with Mean Value.
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes))] <- round(mean(df$Arrival.Delay.in.Minutes, na.rm=TRUE))
#Replacing Flight.time.in.minutes NA's with Mean Value.
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes))] <- round(mean(df$Flight.time.in.minutes, na.rm=TRUE))

##TRANSFORMATIONS on columns##
df$Airline.Status <- as.numeric(as.factor(df$Airline.Status))
df$Gender <- as.numeric(as.factor(df$Gender))
df$Type.of.Travel <- as.numeric(as.factor(df$Type.of.Travel))
df$Class <- as.numeric(as.factor(df$Class))
df$Partner.Name <- as.numeric(as.factor(df$Partner.Name))
df$Origin.State <- as.numeric(as.factor(df$Origin.State))
df$Destination.State <- as.numeric(as.factor(df$Destination.State))
df$Flight.cancelled <- as.numeric(as.factor(df$Flight.cancelled))

#Adding caret package to library
library(caret)
#Creating train and test data set.
trainDataIndex <- createDataPartition(df$typeofCust, p=0.70, list=FALSE)
trainData <- df[trainDataIndex,]
testData <- df[-trainDataIndex,]

## LINEAR REGRESSION MODEL - 3 Level classfication ##
linearModel <- lm(formula=Likelihood.to.recommend ~ Airline.Status+Age+Gender+
                    Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                    Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                    Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                    Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                    Flight.time.in.minutes+Flight.Distance, data=trainData)
#Summary for linear model
summary(linearModel)
#Prediciting on test data from linear model
predictedLM <- predict(linearModel, testData)
#Transforming the predicted likelihood score into target categories.
predictedCat <- cut(predictedLM, breaks=c(-Inf,6,8,Inf), labels=c("Detractors", "Passive", "Promoters"))
#Accuracy - 59.57%
#Confusion matrix
confLM <- confusionMatrix(predictedCat, testData$typeofCust, mode="prec_recall", positive="1")
#Variable importance
varImp(linearModel)



## LOGISTIC REGRESSION - 2 level classification##
logitModel <- glm(formula=dummytypeCust~Airline.Status+Age+Gender+
                    Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                    Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                    Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                    Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                    Flight.time.in.minutes+Flight.Distance, data=trainData, family="binomial")
#Summary for logistic model
summary(logitModel)
#Predicting on testData
predictedLogit <- predict(logitModel, testData)
#Converting probabilities to 1 for >0.5 and 0 for <0.5
y_pred_num <- ifelse(predictedLogit<0.5, 1, 0)
#Converting predicted variable to factor.
y_pred <- factor(y_pred_num, levels=c(1,0))
y_pred <- as.factor(y_pred_num)
#Actual type of customer for trainData.
y_act <- testData$dummytypeCust
#Calculating accuracy - 78%
accuracy <- mean(y_pred==y_act)
#Adding e1071 package to R library
library(e1071)
#Creating confusion matrix / calcualting F1 score, precision, recall.
confMat <- confusionMatrix(y_pred, y_act, mode="prec_recall", positive="1")



## RANDOM FOREST - 3 level classification##
#Adding randomForest package to R library
library(randomForest)
rf <- randomForest(typeofCust ~ Airline.Status+Age+Gender+
                     Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                     Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                     Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                     Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                     Flight.time.in.minutes+Flight.Distance, data=trainData)
#Predicting on test data
predictedRF <- predict(rf, testData)
#Accuracy = 66%
#Confusion matrix
confRF <- confusionMatrix(predictedRF, testData$typeofCust, mode="prec_recall")
#Variable Importance
varImp(rf)




## RANDOM FOREST -2 LEVEL CLASSIFICATION##
rf2 <- randomForest(dummytypeCust ~ Airline.Status+Age+Gender+
                      Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                      Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                      Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                      Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                      Flight.time.in.minutes+Flight.Distance, data=trainData)
#Predicting on testData
predictedRF2 <- predict(rf2, testData)
#Accuracy = 83.11%
#Confusion matrix
confRF2 <- confusionMatrix(predictedRF2, testData$dummytypeCust, mode="prec_recall")


## SVM - 3 level classification##
library(kernlab)
svmModel <- ksvm(typeofCust ~ Airline.Status+Age+Gender+
                   Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                   Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                   Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                   Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                   Flight.time.in.minutes+Flight.Distance, 
                 data=trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#Predicting on test data
predictedSVM <- predict(svmModel, testData)
#Accuracy- 59.94%
#Confusion matrix
confSVM <- confusionMatrix(predictedSVM, testData$typeofCust, mode="prec_recall")
#Variable importance
varImp(svmModel)



## SVM - 2 LEVEL CLASSIFICATION
svmModel2 <- ksvm(dummytypeCust ~ Airline.Status+Age+Gender+
                    Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                    Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                    Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                    Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                    Flight.time.in.minutes+Flight.Distance, data=trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#Predicting on test data
predictedSVM2 <- predict(svmModel2, testData)
#Accuracy- 79.83%
#Confusion matrix
confSVM2 <- confusionMatrix(predictedSVM2, testData$dummytypeCust, mode="prec_recall")



## DECISION TREES - 3 level classification##
#Adding rpart package to R library
library(rpart)
#Installing rpart.plot package 
install.packages("rpart.plot")
#Adding rpart.plot package to R library
library(rpart.plot)
cartTree <- rpart(typeofCust ~ Airline.Status+Age+Gender+
                    Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                    Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                    Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                    Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                    Flight.time.in.minutes+Flight.Distance, data=trainData, method="class")
#Prediciting on test data
predictedCART <- predict(cartTree, testData, type="class")
#Accuracy - 60.36%
#Confusion matrix
confCART <- confusionMatrix(predictedCART, testData$typeofCust, mode="prec_recall")
#Variable importance
varImp(cartTree)



## DECISION TREES - 2 LEVEL CLASSIFICATION
cartTree2 <- rpart(dummytypeCust ~ Airline.Status+Age+Gender+
                     Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+Type.of.Travel+
                     Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+
                     Class+Day.of.Month+Partner.Name+Origin.State+Destination.State+Scheduled.Departure.Hour+
                     Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.cancelled+
                     Flight.time.in.minutes+Flight.Distance, data=trainData, method="class")
#Prediciting on test data
predictedCART2 <- predict(cartTree2, testData, type="class")
#Accuracy - 79.96%
#Confusion matrix
confCART2 <- confusionMatrix(predictedCART2, testData$dummytypeCust, mode="prec_recall")



