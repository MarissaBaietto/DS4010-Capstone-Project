##--Reading in the crash data----
library(readr)

Crashes_1 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_1.csv")
Crashes_2 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_2.csv")
Crashes_3 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_3.csv")
Crashes_4 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_4.csv")
Crashes_5 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_5.csv")
Crashes_6 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_6.csv")
Crashes_7 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_7.csv")
Crashes_8 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_8.csv")
Crashes_9 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_9.csv")
Crashes_10 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_10.csv")
Crashes_11 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_11.csv")
Crashes_12 <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Vehicle_Crashes_Part_12.csv")

Crashes <- rbind(Crashes_1, Crashes_2, Crashes_3, Crashes_4, Crashes_5, Crashes_6, Crashes_7, Crashes_8, Crashes_9, 
                 Crashes_10, Crashes_11, Crashes_12)

##--Cleaning the crash data----
library(tidyverse)

##Removing Variables Not Interested In
Crashes <- Crashes %>% select(-c("Law.Enforcement.Case.Number","DOT.District","Route.with.System",
                                 "Location.Description","Location.of.First.Harmful.Event","Roadway.Contribution",
                                 "Roadway.Type", "Roadway.Surface", "Travel.Direction"))

## Recoding Variables
Crashes$Month.of.Crash <- recode(Crashes$Month.of.Crash, 
                                 "01-January" = "Jan", 
                                 "02-February" = "Feb",
                                 "03-March" = "Mar",
                                 "04-April" = "Apr",
                                 "05-May" = "May",
                                 "06-June" = "Jun",
                                 "07-July" = "Jul",
                                 "08-August" = "Aug",
                                 "09-September" = "Sep",
                                 "10-October" = "Oct",
                                 "11-November" = "Nov",
                                 "12-December" = "Dec")

Crashes$Day.of.Week <- recode(Crashes$Day.of.Week, 
                              "1-Sunday" = "Sun", 
                              "2-Monday" = "Mon",
                              "3-Tuesday" = "Tue",
                              "4-Wednesday" = "Wed",
                              "5-Thursday" = "Thu",
                              "6-Friday" = "Fri",
                              "7-Saturday" = "Sat")

Crashes$Year <- substring(Crashes$Date.of.Crash, first = 7, last = 10)


##--Reading in the vehicle data----
Vehicle <- read.csv("C:/Users/mbaie/OneDrive - Iowa State University/Spring 2025/DS 401/Crash_Vehicle_Data_(SOR).csv")

##--Cleaning the vehicle data----

## Choosing the variables of interest
Vehicle <- Vehicle %>% select(-c("X", "Y", "OBJECTID", "VEH_CRASH_KEY", "VEH_UNITKEY",
                                 "DRUGTEST", "DRIVERCOND", "VISIONOBS", "DCONTCIRC1", 
                                 "DCONTCIRC2", "VCONFIG", "CARGOBODY", "VYEAR", "STYLE",
                                 "VLP_STATE", "OCCUPANTS", "VACTION", "SEQEVENTS1", "SEQEVENTS2",
                                 "SEQEVENTS3", "SEQEVENTS4", "MOSTHARM", "TRAFCONT", "FIXOBJSTR",
                                 "MOSTDAMAGE", "DAMAGE", "CSEVERITY", "MAJORCAUSE", "CSURFCOND",
                                 "DRUGALCREL", "ROADTYPE", "WZ_RELATED", "FATALITIES", "CRASH_YEAR",               
                                 "XCOORD", "YCOORD", "FROM_MEASURE", "TO_MEASURE", "ROUTEID", "CRASH_DATETIME",           
                                 "CRASH_DATETIME_UTC", "CRASH_DATETIME_UTC_OFFSET", "REST_UPDATED",             
                                 "REST_UPDATED_UTC_OFFSET", "GLOBALID"))

## Fixing Outliers and NA
Vehicle$DRIVERAGE <- ifelse(Vehicle$DRIVERAGE <= 102, Vehicle$DRIVERAGE,NA)
Vehicle$DRIVERAGE <- ifelse(Vehicle$DRIVERAGE >= 6)

Vehicle$DRIVERGEN <- ifelse(Vehicle$DRIVERGEN %in% c("F","M"), Vehicle$DRIVERGEN, NA)

Vehicle$DL_STATE <- ifelse(Vehicle$DL_STATE == "XX", NA, Vehicle$DL_STATE)

Vehicle$CHARGED <- ifelse(Vehicle$CHARGED == 1, 1, NA)

#Anything over 0.4 is fatal
Vehicle$ALCRESULT <- ifelse(Vehicle$ALCRESULT >=0 & Vehicle$ALCRESULT <= 0.5, Vehicle$ALCRESULT, NA)

Vehicle$DRUGRESULT <- ifelse(Vehicle$DRUGRESULT == 77, NA, Vehicle$DRUGRESULT)

Vehicle$SPEEDLIMIT <- ifelse(Vehicle$SPEEDLIMIT %in% c(NA, 1,2,4,99,777), NA, Vehicle$SPEEDLIMIT)

## Making a new variable called "HasVehicleData"

Vehicle$HasVehicleData <- "Yes"

##--Merging crashes and vehicle----
## Full dataset
Full_Dataset <- left_join(Crashes, Vehicle, by = c("Iowa.DOT.Case.Number"="CASENUMBER")) 

## Dataset with only unique crashes
Unique_Crashes <- Full_Dataset[!duplicated(Full_Dataset$Iowa.DOT.Case.Number), ]

Unique_Crashes$Fatal <- ifelse(Unique_Crashes$Crash.Severity == "Fatal", 1, 0)
Unique_Crashes$Work.Zone <- ifelse(Unique_Crashes$Work.Zone == "Yes", "Yes", "No")

write.csv(Unique_Crashes, "Unique Crashes.csv")

library(ggplot2)
##--Making maps data set----
library(mapdata)
library(maps)
state <- map_data("state")
iowa <- subset(state, region=="iowa")
counties <- map_data("county")
iowa_county <- subset(counties, region=="iowa")

county_num_crashes <- Unique_Crashes %>% mutate(County.Name = tolower(County.Name)) %>% group_by(County.Name, Year) %>% summarise("Number of Crashes" = n())

iowa_county_merged <- left_join(iowa_county, county_num_crashes, by = c("subregion" = "County.Name"))

write.csv(iowa_county_merged, "Iowa Counties.csv")

