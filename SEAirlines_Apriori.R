# Change to the folder containing your homework data files
setwd("~/Vicky/Fall19/Intro to Data Science/Homework")
library(jsonlite)
library(tidyverse)
#Creating dataframe from JSON
df<-fromJSON("fall2019-survey-M03.json")
#Creating another dataframe from JSON
dfRAW <- fromJSON("fall2019-survey-M03.json")


## ASSOCIATION RULES ##
#Adding arules and arulesViz package to R library. 
library(arules)
library(arulesViz)


#Creating a new data frame for apriori ruleset
dfAR <- data.frame(df$Age,df$Type.of.Travel,df$Flights.Per.Year,df$Eating.and.Drinking.at.Airport,df$Partner.Name,dfRAW$Origin.State,dfRAW$Airline.Status,df$Arrival.Delay.in.Minutes,df$Flight.Distance,df$Loyalty,df$typeofCust)
#Transformations on the columns
dfAR$df.Age <- cut(dfAR$df.Age, breaks=c(-Inf,30,60,Inf), labels=c("15 to 30", "30 to 60", "60 to 85"))
dfAR$df.Type.of.Travel <- cut(dfAR$df.Type.of.Travel, breaks=c(-Inf,1,2,Inf), labels=c("Business travel", "Mileage tickets", "Personal Travel"))
dfAR$df.Flights.Per.Year <- cut(dfAR$df.Flights.Per.Year, breaks=c(-Inf,30,60,Inf), labels=c("Below 30", "30 to 60", "60 to 100"))
dfAR$df.Eating.and.Drinking.at.Airport <- cut(dfAR$df.Eating.and.Drinking.at.Airport, breaks=c(-Inf,100,Inf), labels=c("Spends below 100", "Spends above 100"))
dfAR$df.Partner.Name <- cut(dfAR$df.Partner.Name, breaks=c(-Inf,1,2,3,4,5,6,7,8,9,10,11,12,13,Inf), labels=c("Cheapseats Airlines Inc.", "Cool&Young Airlines Inc.", "EnjoyFlying Air Services","FlyFast Airways Inc.","FlyHere Airways","FlyToSun Airlines Inc.","GoingNorth Airlines Inc.","Northwest Business Airlines Inc.","OnlyJets Airlines Inc.","Oursin Airlines Inc.","Paul Smith Airlines Inc.","Sigma Airlines Inc.","Southeast Airlines Co.","West Airways Inc."))
dfAR$df.Arrival.Delay.in.Minutes <- cut(dfAR$df.Arrival.Delay.in.Minutes, breaks=c(-Inf,30,60,Inf), labels=c("Less than 30 min", "30 to 60 min", "More than 1 hour"))
dfAR$df.Loyalty <- cut(dfAR$df.Loyalty, breaks=c(-Inf,0,Inf), labels=c("Negative Loyality Score", "Positive Loyality Score"))
dfAR$df.Flight.Distance <- cut(dfAR$df.Flight.Distance, breaks=c(-Inf,1000,2000,Inf), labels=c("Short", "Medium", "Long"))
#Creating a transactions matrix
dfARX <- as(dfAR,"transactions")

#Ruleset for Detractors
ruleset1 <- apriori(dfARX,
                    parameter = list(support=0.13, confidence=0.5),
                    appearance = list(default="lhs", rhs=("df.typeofCust=Detractors")))
# Inspect ruleset1
inspect(ruleset1)
inspectDT(ruleset1)

# Implementing more association rules
Age_Group <- cut(df$Age, breaks = c(10,20,40,60,85), labels = c("Youth", "Adults", "Old","Senior Citizens"))
# Converting to factors all the character, numeric and integer variables into before converting the dataframe into transactions
df3_whole = df3_whole %>% mutate_if(is.numeric, as.factor)

df3_whole = df3_whole %>% mutate_if(is.integer, as.factor)

df3_whole = df3_whole %>% mutate_if(as.character, as.factor)

df3_ fly1 = df3_ fly1 %>% mutate_if(is.numeric, as.factor)

df3_ fly1 = df3_ fly1 %>% mutate_if(is.integer, as.factor)

df3_ fly1 = df3_ fly1 %>% mutate_if(as.character, as.factor)

# Apriori Code 

dfARX_whole <- as(df3_whole,"transactions") # Converting the dataframe into Trasactions for whole dataset(rows and only selected columns)

dfARX_flyfast <- as(df3_fly1,"transactions") # Converting the dataframe into Transactions for FlyFast Airways Inc. 

# Applying Apriori Algorithm to get the skeleton of the detractors based on the significant attributes identified by the various models used
ruleset_det <- apriori(df3_fly1,
                       parameter = list(support=0.06, confidence=0.8),
                       appearance = list(default="lhs", rhs=("df3_fly.npscategory=Detractor")))
inspect(ruleset_det) # To view the rules created by the Apriori Algorithm
InspectDT(ruleset_det) # To view the rules in an interactive manner using inspectDT function
# Applying Apriori Algorithm to get the outline of detractors from the whole dataset based on significant attributes and varifying the results on this whole dataset
ruleset_det_limit <- apriori(dfARX_whole,
                             parameter = list(support=0.02, confidence=0.5),
                             appearance = list(lhs=c("df.Type.of.Travel=Personal Travel","df.Airline.Status=Blue","df.Origin.State=Texas"), rhs=("df.npscategory=Detractor")))

inspectDT(ruleset_det_limit)

# Taking only Cheapseats airlines data from the whole dataset

df3_cheap <- df %>% filter(df$Partner.Name=="Cheapseats Airlines Inc.")

# Including only significant factors in the dataframe for cheapseats airlines

df3_cheap1 <- data.frame(df3_cheap$Partner.Name,df3_cheap$Airline.Status,df3_cheap$Age,df3_cheap$Type.of.Travel,df3_cheap$Eating.and.Drinking.at.Airport,df3_cheap$Origin.State,df3_cheap$Destination.State,df3_cheap$Departure.Delay.in.Minutes,df3_cheap$Arrival.Delay.in.Minutes,df3_cheap$Loyalty,df3_cheap$npscategory)

# Converting to factors all the character, numeric and integer variables into before converting the dataframe into transactions

df3_cheap1 = df3_cheap1 %>% mutate_if(is.numeric, as.factor)

df3_cheap1 = df3_cheap1 %>% mutate_if(is.integer, as.factor)

df3_cheap1 = df3_cheap1 %>% mutate_if(as.character, as.factor)

dfARX_cheap <- as(df3_cheap1,"transactions") # Converting the dataframe into Transactions for Cheapseats Airlines Inc.

# Applying Apriori Algorithm to get the skeleton of the detractors based on the significant attributes identified by the various models used

ruleset_det_cheap <- apriori(dfARX_cheap,
                             parameter = list(support=0.04, confidence=0.8),
                             appearance = list(default="lhs", rhs=("df3_cheap.npscategory=Detractor")))

inspectDT(ruleset_det_cheap) # To view the rules in an interactive manner 

