setwd("C:\\Deeps\\R")
getwd()

#  What has been the trend of gun violence in the past few years?
#  What states have the highest incidents per capita per year? How has this metric changed over time?
#  Are officer involved shootings on the rise? Where are they most concentrated? Do they correlate with the rates of accidental deaths and mass shootings?

# Load all the data in the dataset

crimeData2014 = read.csv("Data/gun-violence-database/mass_shootings_2014.csv")
crimeData2015 = read.csv("Data/gun-violence-database/mass_shootings_2015.csv")
crimeData2016 = read.csv("Data/gun-violence-database/mass_shootings_2016.csv")

# Clean the data
crimeData2014 = na.omit(crimeData2014)
crimeData2015 = na.omit(crimeData2015)
crimeData2016 = na.omit(crimeData2016)

# Merge the data
crimeData = rbind(crimeData2014, crimeData2015)
crimeData = rbind(crimeData, crimeData2016)

# Extract Year from COmplete Date
library(lubridate)
completeDate=mdy(crimeData$Incident.Date)
crimeData$Incident.Date = year(completeDate)

# Task 1: What has been the trend of gun violence in the past few years?

gunviolence = crimeData[, c(1,5,6)]
gunviolence$Incident.Date = as.integer(gunviolence$Incident.Date)

aggregateKilled = aggregate(gunviolence$X..Killed, by=list(Category=gunviolence$Incident.Date), FUN=sum)
aggregateKilled
plot(aggregateKilled, xlab = "Year", ylab = "People Killed")

aggregateInjured = aggregate(gunviolence$X..Injured, by=list(Category=gunviolence$Incident.Date), FUN=sum)
aggregateInjured
plot(aggregateInjured, xlab = "Year", ylab = "People Injured")


#  What states have the highest incidents per capita per year? 
#  How has this metric changed over time?

crimeDataWithStateName = crimeData[, c(1,2, 5,6)]
crimeDataWithStateName$Incident.Date = as.integer(gunviolence$Incident.Date)


aggregateKilledWithStateName = aggregate(crimeDataWithStateName$X..Killed, by=list(Year=crimeDataWithStateName$Incident.Date, State = crimeDataWithStateName$State), FUN=sum)
aggregateKilledWithStateName
plot(aggregateKilled, xlab = "Year", ylab = "People Killed")
