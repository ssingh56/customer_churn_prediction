dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment

# Import dataset

# Set working directory, change it to your work folder:
setwd("~/Google Drive/ADS Master/2019 Fall/IST687/Final_Project")
# Adding jsonlite to our R library
library(jsonlite)
# Creating a dataframe from the json data.
df <- jsonlite::fromJSON("fall2019-survey-M03.json")
# Structure of the dataset
str(df)
# Summary of the dataset
summary(df)
# Viewing the dataset
View(df)


# Data munging

library(tidyverse)

# Convert approriate variables to factors:
df$Airline.Status<-as.factor(df$Airline.Status)
df$Type.of.Travel<-as.factor(df$Type.of.Travel)
df$Class<-as.factor(df$Class)
df$Flight.cancelled<-as.factor(df$Flight.cancelled)
df$Partner.Code<-as.factor(df$Partner.Code)
df$Partner.Name<-as.factor(df$Partner.Name)
df$Gender<-as.factor(df$Gender)

# Convert flight date to date:
df$Flight.date <- as.Date(df$Flight.date, "%m/%d/%Y")

#Remove State Abbreviations from destination and origin cit fields:
df$Origin.City<-gsub(", ..","",df$Origin.City)
df$Destination.City<-gsub(", ..","",df$Destination.City)

# Replace the NA in Likelyhood to Recommend with mean
df$Likelihood.to.recommend[which(is.na(df$Likelihood.to.recommend))] <- round(mean(df$Likelihood.to.recommend, na.rm = TRUE))
# Add a column of promoters and calculate overall NPS
df$Promoters <- ifelse(df$Likelihood.to.recommend > 8, "Promoter", ifelse(df$Likelihood.to.recommend < 7, "Detractor", "Passive"))


# Linear modeling

library(corrplot)

# Variables contain numerical data: Age, Price.Sensitivity, Year.of.First.Flight, Flights.Per.Year, Loyalty, Total.Freq.Flyer.Accts,
# Shopping.Amount.at.Airport, Eating.and.Drinking.at.Airport, Day.of.Month, Scheduled.Departure.Hour, Departure.Delay.in.Minutes, 
# Arrival.Delay.in.Minutes, Flight.time.in.minutes, Flight.Distance, Likelihood.to.recommend, olong, olat, dlong, dlat

# Create correlation matrix:
dfNum <- data.frame(df$Age, df$Price.Sensitivity, df$Year.of.First.Flight, df$Flights.Per.Year, df$Loyalty, df$Total.Freq.Flyer.Accts,
                    df$Shopping.Amount.at.Airport, df$Eating.and.Drinking.at.Airport, df$Day.of.Month, df$Scheduled.Departure.Hour, 
                    df$Flight.Distance, df$Likelihood.to.recommend, df$olong, df$olat, df$dlong, df$dlat)
corMat <- cor(dfNum)
# Plot:
corrplot(corMat, tl.cex = 0.8, tl.col = "black", tl.srt = 45)

# Linear model for the whole dataset:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = df))
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes+Flight.Distance, data = df))

# Part 1: Separate the dataset based on whether flight cancelled or not
# Separate dataset:
# Dataframe without NAs for both departure and arrival:
noFlightNAs <- df[-which(is.na(df$Departure.Delay.in.Minutes)|is.na(df$Arrival.Delay.in.Minutes)),]
# No flight was cancelled in the dataframe without NAs for both departure and arrival:
which(noFlightNAs$Flight.cancelled == "Yes")
# Data for all Flight cancelled passengers from the orginal dataframe:
flightCancelled <- df[which(df$Flight.cancelled == "Yes"),]
# Data for all Flight not cancelled passengers from the orginal dataframe:
flightNotCancelled <- df[which(df$Flight.cancelled == "No"),]
# Data for all departure and arrival delayed passengers:
bothDelay <- noFlightNAs[which(noFlightNAs$Departure.Delay.in.Minutes > 0 & noFlightNAs$Arrival.Delay.in.Minutes > 0),]
# Data for all departure delayed and arrival not delayed passengers:
depDelayOnly <- noFlightNAs[which(noFlightNAs$Departure.Delay.in.Minutes > 0 & noFlightNAs$Arrival.Delay.in.Minutes == 0),]
# Data for all departure delayed passengers (Do not care whether arrival delayed or not):
depDelay <- noFlightNAs[which(noFlightNAs$Departure.Delay.in.Minutes > 0),]
# Data for all departure not delayed and arrival delayed passengers:
arrDelayOnly <- noFlightNAs[which(noFlightNAs$Departure.Delay.in.Minutes == 0 & noFlightNAs$Arrival.Delay.in.Minutes > 0),]
# Data for all departure not delayed and arrival not delayed passengers:
notDelay <- noFlightNAs[which(noFlightNAs$Departure.Delay.in.Minutes == 0 & noFlightNAs$Arrival.Delay.in.Minutes == 0),]

# Lineal models for all the meaningful variables:
# For cancelled flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month, data = flightCancelled))
# For all not cancelled flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = flightNotCancelled))
# For both departure and arrival delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = bothDelay))
# For only departure delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = depDelayOnly))
# For all the departure delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = depDelay))
# For only arrival delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = arrDelayOnly))
# For both departure and arrival not delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Flight.time.in.minutes+Flight.Distance, data = notDelay))

# Lineal model for cancelled flights:
summary(lm(formula = Likelihood.to.recommend ~ Age, data = flightCancelled))
# Lineal model for all not cancelled flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes+Flight.Distance, data = flightNotCancelled))
# Lineal model for both departure and arrival delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport, data = bothDelay))
# Lineal model for only departure delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Eating.and.Drinking.at.Airport+
             Day.of.Month+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Flight.time.in.minutes, data = depDelayOnly))
# Lineal model for all the departure delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Eating.and.Drinking.at.Airport, data = depDelay))
# Lineal model for only arrival delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Scheduled.Departure.Hour+
             Arrival.Delay.in.Minutes, data = arrDelayOnly))
# Lineal model for both departure and arrival not delayed flights:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport+Scheduled.Departure.Hour, data = notDelay))

# Flight departure delayed, shopping amount at the airport and eating & drinking at the airport:
summary(lm(formula = Shopping.Amount.at.Airport ~ Departure.Delay.in.Minutes+Eating.and.Drinking.at.Airport, data = depDelay))
summary(lm(formula = Eating.and.Drinking.at.Airport ~ Departure.Delay.in.Minutes+Shopping.Amount.at.Airport, data = depDelay))
# Shopping amount at airport and passage info:
summary(lm(formula = Shopping.Amount.at.Airport ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport+Flight.Distance, data = depDelay))
summary(lm(formula = Shopping.Amount.at.Airport ~ Price.Sensitivity+Flights.Per.Year+Loyalty+Eating.and.Drinking.at.Airport,
           data = depDelay))
# Eating & drinking at airport and passage info:
summary(lm(formula = Eating.and.Drinking.at.Airport ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty+Total.Freq.Flyer.Accts
           +Flight.Distance, data = depDelay))
summary(lm(formula = Eating.and.Drinking.at.Airport ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty, data = depDelay))


# Part 2: Separate the dataset based on gender
# Separate dataset:
female <- df[which(df$Gender == "Female"),]
male <- df[which(df$Gender == "Male"),]

# Lineal models for all the meaningful variables:
# For females:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = female))
# For males:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = male))

# Lineal model for females:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport+Day.of.Month+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data = female))
# Lineal model for males:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+
             Eating.and.Drinking.at.Airport+Day.of.Month+Flight.Distance, data = male))


# Part 3: Separate the dataset based on age
# Separate dataset:
age11_20 <- df[which((df$Age > 10) & (df$Age < 21)),]
age21_30 <- df[which((df$Age > 20) & (df$Age < 31)),]
age31_40 <- df[which((df$Age > 30) & (df$Age < 41)),]
age41_50 <- df[which((df$Age > 40) & (df$Age < 51)),]
age51_60 <- df[which((df$Age > 50) & (df$Age < 61)),]
age61_70 <- df[which((df$Age > 60) & (df$Age < 71)),]
age71_80 <- df[which((df$Age > 70) & (df$Age < 81)),]
age80More <- df[which(df$Age > 80),]

# Lineal models for all the meaningful variables:
# For people age between 11 and 20:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age11_20))
# For people age between 21 and 30:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age21_30))
# For people age between 31 and 40:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age31_40))
# For people age between 41 and 50:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age41_50))
# For people age between 51 and 60:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age51_60))
# For people age between 61 and 70:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age61_70))
# For people age between 71 and 80:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = ageage71_80))
# For people age over 80:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = age80More))

# Lineal model for people age between 11 and 20:
summary(lm(formula = Likelihood.to.recommend ~ Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport, data = age11_20))
# Lineal model for people age between 21 and 30:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport, data = age21_30))
# Lineal model for people age between 31 and 40:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Flights.Per.Year+Loyalty+Total.Freq.Flyer.Accts+
             Eating.and.Drinking.at.Airport, data = age31_40))
# Lineal model for people age between 41 and 50:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Flights.Per.Year+Loyalty+Eating.and.Drinking.at.Airport+
             Flight.time.in.minutes+Flight.Distance, data = age41_50))
# Lineal model for people age between 51 and 60:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Flights.Per.Year+Loyalty, data = age51_60))
# Lineal model for people age between 61 and 70:
summary(lm(formula = Likelihood.to.recommend ~ Price.Sensitivity+Flights.Per.Year+Loyalty+Arrival.Delay.in.Minutes, data = age61_70))
# Lineal model for people age between 71 and 80:
summary(lm(formula = Likelihood.to.recommend ~ Flights.Per.Year+Arrival.Delay.in.Minutes, data = age71_80))
# Lineal model for people age over 80:
summary(lm(formula = Likelihood.to.recommend ~ Scheduled.Departure.Hour, data = age80More))


# Part 4: Separate the dataset based on airline statuses
# Separate dataset:
blue <- df[which(df$Airline.Status == "Blue"),]
silver <- df[which(df$Airline.Status == "Silver"),]
gold <- df[which(df$Airline.Status == "Gold"),]
platinum <- df[which(df$Airline.Status == "Platinum"),]

# Lineal models for all the meaningful variables:
# For blue:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = blue))
# For silver:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = silver))
# For gold:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = gold))
# For platinum:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = platinum))

# Lineal model for people's airline status is blue:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes, data = blue))
# Lineal model for people's airline status is silver:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Loyalty+Total.Freq.Flyer.Accts+Flight.time.in.minutes+Flight.Distance, data = silver))
# Lineal model for people's airline status is gold:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Loyalty, data = gold))
# Lineal model for people's airline status is platinum:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data = platinum))


# Part 5: Separate the dataset based on type of travel
# Separate dataset:
businessTravel <- df[which(df$Type.of.Travel == "Business travel"),]
personalTravel <- df[which(df$Type.of.Travel == "Personal Travel"),]
mileageTickets <- df[which(df$Type.of.Travel == "Mileage tickets"),]

# Lineal models for all the meaningful variables:
# For business travel:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = businessTravel))
# For personal travel:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = personalTravel))
# For mileage tickets:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = mileageTickets))

# Lineal model for business travel:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Shopping.Amount.at.Airport+
             Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes, data = businessTravel))
# Lineal model for personal travel:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport+
             Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data = personalTravel))
# Lineal model for mileage tickets:
summary(lm(formula = Likelihood.to.recommend ~ Flights.Per.Year+Eating.and.Drinking.at.Airport, data = mileageTickets))


# Part 6: Separate the dataset based on classes
# Separate dataset:
ecoClass <- df[which(df$Class == "Eco"),]
ecoPlusClass <- df[which(df$Class == "Eco Plus"),]
businessClass <- df[which(df$Class == "Business"),]

# Lineal models for all the meaningful variables:
# For Eco class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = ecoClass))
# For Eco Plus class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = ecoPlusClass))
# For Business class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = businessClass))

# Lineal model for Eco class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes+Flight.Distance, data = ecoClass))
# Lineal model for Eco Plus class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+Day.of.Month, data = ecoPlusClass))
# Lineal model for Business class:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Eating.and.Drinking.at.Airport, data = businessClass))
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Eating.and.Drinking.at.Airport, data = businessClass))


# Part 7: Separate the dataset based on partner name
# Separate dataset:
# West Airways Inc. will not be considered because it only have 13 responses.
FFA <- df[which(df$Partner.Name == "FlyFast Airways Inc."),]
CSA <- df[which(df$Partner.Name == "Cheapseats Airlines Inc."),]
PSA <- df[which(df$Partner.Name == "Paul Smith Airlines Inc."),]
EFA <- df[which(df$Partner.Name == "EnjoyFlying Air Services"),]
NBA <- df[which(df$Partner.Name == "Northwest Business Airlines Inc."),]
STA <- df[which(df$Partner.Name == "Southeast Airlines Co."),]
SGA <- df[which(df$Partner.Name == "Sigma Airlines Inc."),]
OSA <- df[which(df$Partner.Name == "Oursin Airlines Inc."),]
FHA <- df[which(df$Partner.Name == "FlyHere Airways"),]
OJA <- df[which(df$Partner.Name == "OnlyJets Airlines Inc."),]
CYA <- df[which(df$Partner.Name == "Cool&Young Airlines Inc."),]
FTA <- df[which(df$Partner.Name == "FlyToSun Airlines Inc."),]
GNA <- df[which(df$Partner.Name == "GoingNorth Airlines Inc."),]

# Lineal models for all the meaningful variables:
# For FlyFast Airways Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = FFA))
# For Cheapseats Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = CSA))
# For Paul Smith Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = PSA))
# For EnjoyFlying Air Services:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = EFA))
# For Northwest Business Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = NBA))
# For Southeast Airlines Co.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = STA))
# For Sigma Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = SGA))
# For Oursin Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = OSA))
# For FlyHere Airways:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = FHA))
# For OnlyJets Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = OJA))
# For Cool&Young Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = CYA))
# For FlyToSun Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = FTA))
# For GoingNorth Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Total.Freq.Flyer.Accts+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = GNA))

# Lineal model for FlyFast Airways Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data = FFA))
# Lineal model for Cheapseats Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Loyalty+
             Eating.and.Drinking.at.Airport+Scheduled.Departure.Hour, data = CSA))
# Lineal model for Paul Smith Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Loyalty, data = PSA))
# Lineal model for EnjoyFlying Air Services:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year, data = EFA))
# Lineal model for Northwest Business Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts, data = NBA))
# Lineal model for Southeast Airlines Co.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Year.of.First.Flight+Flights.Per.Year+Arrival.Delay.in.Minutes, data = STA))
# Lineal model for Sigma Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Eating.and.Drinking.at.Airport+
             Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data = SGA))
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Eating.and.Drinking.at.Airport+Arrival.Delay.in.Minutes, data = SGA))
# Lineal model for Oursin Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Total.Freq.Flyer.Accts+Eating.and.Drinking.at.Airport+
             Arrival.Delay.in.Minutes, data = OSA))
# Lineal model for FlyHere Airways:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Flights.Per.Year+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data = FHA))
# Lineal model for OnlyJets Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Day.of.Month, data = OJA))
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year, data = OJA))
# Lineal model for Cool&Young Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Loyalty+Flight.time.in.minutes+Flight.Distance, data = CYA))
summary(lm(formula = Likelihood.to.recommend ~ Age+Flights.Per.Year+Loyalty+Flight.time.in.minutes, data = CYA))
# Lineal model for FlyToSun Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Age+Price.Sensitivity+Eating.and.Drinking.at.Airport, data = FTA))
# Lineal model for GoingNorth Airlines Inc.:
summary(lm(formula = Likelihood.to.recommend ~ Flights.Per.Year, data = GNA))



