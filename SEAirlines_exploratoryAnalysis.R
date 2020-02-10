# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Checking the working directory 
# If incorrect change to the folder containing your homework data files
getwd()

#Download and install tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Downlod and install jsonlite library
#install.packages("jsonlite")
library(jsonlite)
library(ggplot2)

#Load data from json into datafram
df<-fromJSON("fall2019-survey-M03.json")
#Explore structure of imported data
str(df)
summary(df)
View(df)

##Total Na in colmns
View(dfPlotNA) <- data.frame(colnames(dfOrig),colSums(is.na(dfOrig)))

barPartnerName <- ggplot(dfPlotNA, aes(x=colnames.dfOrig., y=colSums.is.na.dfOrig..) )
barPartnerName <- barPartnerName + geom_bar(fill="Blue", stat = "identity")
barPartnerName <- barPartnerName + theme(axis.text.x = element_text(angle=90, hjust=1))
barPartnerName

############cleaning and filling na #############

#Likelihood.to.recommend has one na
#Removing it
df<- df[!is.na(df$Likelihood.to.recommend),]

#Filling Departure.Delay.in.Minutes and Arrival.Delay.in.Minutes with median
df$Departure.Delay.in.Minutes[is.na(df$Departure.Delay.in.Minutes)]<-median(df$Departure.Delay.in.Minutes,na.rm=TRUE)
df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes)]<-median(df$Arrival.Delay.in.Minutes,na.rm=TRUE)
df$Flight.time.in.minutes[is.na(df$Arrival.Delay.in.Minutes)]<-mean(df$Arrival.Delay.in.Minutes,na.rm = TRUE)



#Adding Promoter, Detractor and Passive columns
df$Promoter <-df$Likelihood.to.recommend>8
df$Detractor <-df$Likelihood.to.recommend<7
df$Passive <-(df$Likelihood.to.recommend>6)&(df$Likelihood.to.recommend<9)
summary(df$Promoter)
summary(df$Passive)
summary(df$Detractor)
#Categorizing Likelihood into Promoter, Detractor and Passive and saving them in a column
df$typeofCust <- cut(df$Likelihood.to.recommend, breaks=c(-Inf,6,8,Inf), labels=c("Detractor", "Passive", "Promoter"))


# Defining a function which calculate nps score of categorical column of a dataframe 
#and plots a bubble plot for it
#ini and fin are min and max size of the bubble in the plot
npsCalculator <- function(df,columnName,ini=3,fin=8){
  #creating new temporary dataframe Npscount having which contains the frequency of promoter passive and detractor 
  NPSCount <- aggregate(df[,c('Promoter','Passive','Detractor')],by=list(df[,columnName]),"sum")  
  # calculating NPs score
  NPSCount$npsScore <- (NPSCount$Promoter - NPSCount$Detractor)*100/(NPSCount$Promoter + NPSCount$Detractor +NPSCount$Passive)
  NPSCount$TotalObservations <- NPSCount$Promoter + NPSCount$Detractor +NPSCount$Passive
  NPSCount$Average <- aggregate(df[,c('Likelihood.to.recommend')],by=list(df[,columnName]),"mean")
  #box plot for NPS
  ggplot(NPSCount, aes(x=reorder(Group.1,npsScore), y=npsScore, size = TotalObservations)) +
    geom_point(alpha=0.3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_size_continuous(range = c(ini, fin)) +
    labs(x = columnName )
  
}

#alalysis plots of Categorical variables

#Partner name
npsCalculator(df,'Partner.Name',1,12)
# Boxplot for Partner Name Vs Likelihood to recommend 
ggplot(aes(y = Likelihood.to.recommend, x = Partner.Name, fill = Partner.Name), data = df) + geom_boxplot() + theme(axis.text.x = element_text(angle=90, hjust=1)) 

#Gender
npsCalculator(df,'Gender')
# Boxplot for Gender Vs Likelihood to recommend 
ggplot(aes(y = Likelihood.to.recommend, x = Gender, fill = Gender), data = df) + geom_boxplot() 


#Destination state
npsCalculator(df,'Destination.State')

#Type of travel
npsCalculator(df,'Type.of.Travel')
# Boxplot for Type of Travel Vs Likelihood to recommend 
ggplot(aes(y = Likelihood.to.recommend, x = Type.of.Travel, fill = Type.of.Travel), data = df) + geom_boxplot() 



#airline status
npsCalculator(df,'Airline.Status')
# Boxplot for Airline Status Vs Likelihood to recommend 
ggplot(aes(y = Likelihood.to.recommend, x = Airline.Status, fill = Airline.Status), data = df) + geom_boxplot() 


#Analysis of Numeric Variables

#flightsper year
npsCalculator(df,'Flights.Per.Year')

#age
# Boxplot for Age Groups Vs Likelihood to recommend 
Age_Group <- cut(df$Age, breaks = c(10,20,40,60,85), labels = c("Youth", "Adults", "Old","Senior Citizens")) 
ggplot(aes(y = Likelihood.to.recommend, x = Age_Group, fill = Age_Group), data = df) + geom_boxplot() 

df$AgeCategory  <-cut(df$Age,breaks = c(0,10,20,30,40,50,60,70,100))
npsCalculator(df,'AgeCategory')

#eating and dring at airport Eating.and.Drinking.at.Airport
summary(df$Eating.and.Drinking.at.Airport)
df$Eating.and.Drinking.Category  <-cut(df$Eating.and.Drinking.at.Airport,breaks = c(-Inf,20,40,60,80,100,999))
npsCalculator(df,'Eating.and.Drinking.Category')

#Arrival.Delay.in.Minutes
summary(df$Arrival.Delay.in.Minutes)
df$ArrivalDelaYCat  <-cut(df$Arrival.Delay.in.Minutes,breaks = c(-Inf,10,30,60,90,120,999))
npsCalculator(df,'ArrivalDelaYCat')


#Loyalty
summary(df$Loyalty)
df$LoyaltyCat  <-cut(df$Loyalty,breaks = c(-1,-0.75,-0.50,-0.25,0,0.25,0.5,0.75,1))
npsCalculator(df,'LoyaltyCat')


#Convert approriate variables to factors
df$Airline.Status<-factor(df$Airline.Status, levels=c("Blue","Silver","Gold","Platinum"), ordered=FALSE)
df$Type.of.Travel<-factor(df$Type.of.Travel,levels = c("Personal Travel","Mileage tickets","Business travel"))
df$Class<-factor(df$Class,levels=c("Eco","Eco Plus","Business"))
df$Flight.cancelled<-as.factor(df$Flight.cancelled)
df$Partner.Code<-as.factor(df$Partner.Code)
df$Partner.Name<-as.factor(df$Partner.Name)
df$Gender<-as.factor(df$Gender)



#Univariate Analysis Of columns 18-24

#OriginState
str(df$Origin.State)
summary(df$Origin.State)
barplot(sort(table(df$Origin.State)),las=2,cex.names =0.32)
averagedf<- aggregate(df$Likelihood.to.recommend, by=list(df$Origin.State), FUN=mean)
averagedf <- averagedf[order(-averagedf$x),]

#DestinationState
str(df$Destination.State)
summary(df$Destination.State)
barplot(sort(table(df$Destination.State)),las=2,cex.names =0.32)
averagedf<- aggregate(df$Likelihood.to.recommend, by=list(df$Destination.State), FUN=mean)
averagedf <- averagedf[order(-averagedf$x),]

#SheduleDeparture Hour
str(df$Scheduled.Departure.Hour)
summary(df$Scheduled.Departure.Hour)
barplot(table(df$Scheduled.Departure.Hour),las=2,cex.names =0.32)
averagedf<- aggregate(df$Likelihood.to.recommend, by=list(df$Scheduled.Departure.Hour), FUN=mean)
qplot(averagedf$Group.1,averagedf$x)
averagedf <- averagedf[order(-averagedf$x),]
averagedf


#Departure.Delay.in.Minutes
str(df$Departure.Delay.in.Minutes)
summary(df$Departure.Delay.in.Minutes)
hist(df$Departure.Delay.in.Minutes)
#qplot(table(df$Departure.Delay.in.Minutes),las=2,cex.names =0.32)
averagedf<- aggregate(df$Departure.Delay.in.Minutes, by=list(df$Likelihood.to.recommend), FUN=mean)
qplot(averagedf$x,averagedf$Group.1)
averagedf <- averagedf[order(-averagedf$x),]
averagedf


#Arrival.Delay.in.Minutes
str(df$Arrival.Delay.in.Minutes)
summary(df$Arrival.Delay.in.Minutes)
hist(df$Arrival.Delay.in.Minutes)
#qplot(table(df$Departure.Delay.in.Minutes),las=2,cex.names =0.32)
averagedf<- aggregate(df$Arrival.Delay.in.Minutes, by=list(df$Likelihood.to.recommend), FUN=mean)
qplot(averagedf$Group.1,averagedf$x)
averagedf <- averagedf[order(-averagedf$x),]
averagedf


#Flight.time.in.minutes
str(df$Flight.time.in.minutes)
summary(df$Flight.time.in.minutes)
hist(df$Flight.time.in.minutes)
#qplot(table(df$Departure.Delay.in.Minutes),las=2,cex.names =0.32)
df$Flight.time.in.minutes[is.na(df$Flight.time.in.minutes)]<-0
averagedf<- aggregate(df$Flight.time.in.minutes, by=list(df$Likelihood.to.recommend), FUN=mean)
qplot(averagedf$x,averagedf$Group.1)
averagedf <- averagedf[order(-averagedf$x),]
averagedf
