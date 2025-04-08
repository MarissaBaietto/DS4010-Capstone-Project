library(readr)
library(dplyr)
Unique_Crashes <- read.csv("Unique Crashes.csv")
Unique_Crashes <- Unique_Crashes %>% filter(Year != 2025)

##--Crash Severity----

##--Amount of Property Damage----
library(glmnet)

### creating sample
sample_index = sample(1 : 10, size = 10, replace = F)



##--Model Creation----
library(readr)
Unique_Crashes <- read.csv("Unique Crashes.csv")

## Random Forest for Fatalities
library(ISLR2)
library(dplyr)
library(randomForest)

## Taking out parameters that will not be included
crashes_variables <- Unique_Crashes %>% select(-c("X.1","X","X.2","Iowa.DOT.Case.Number","Date.of.Crash","Time.of.Crash","Number.of.Fatalities","Location","HasVehicleData", "City.Name", "Fatal"))
## Take out city name, take out ones with null county name
## Make a first harmful event "unknown"
## Take out "Manner.of.Crash.collision" (the NA ones)
## Take out the NA "Major.Cause"
## Make NA and "" "Unknown" for environemntal conditions
## Make "" Unknown for Light Conditions
## make "" unknown for surface conditions
## Make "" unknown for weather conditions
## Make NA unknown for DRIVERGEN
## Make NA and "" unknown for DLSTATE
## Make NA 0 for CHARGED
## Make NA 0 for ALCRESULT
## Make NA 77 for DRUGRESULT, make it a factor variable as well
## Make "" unknown for Make
## Make "" unknown for model
## Make NA "unknown" for SPEEDLIMIT, make it a factor variable as well

crashes_variables$First.Harmful.Event <- ifelse(crashes_variables$First.Harmful.Event == "", "Unknown", crashes_variables$First.Harmful.Event)
crashes_variables <- crashes_variables[crashes_variables$County.Name != "", ]
crashes_variables <- crashes_variables[crashes_variables$Manner.of.Crash.Collision != "", ]
crashes_variables <- crashes_variables[crashes_variables$Major.Cause != "", ]
crashes_variables$Environmental.Conditions <- ifelse(is.na(crashes_variables$Environmental.Conditions) |  crashes_variables$Environmental.Conditions == "", "None apparent", crashes_variables$Environmental.Conditions)
crashes_variables$Light.Conditions <- ifelse(crashes_variables$Light.Conditions == "", "Unknown", crashes_variables$Light.Conditions)
crashes_variables$Surface.Conditions <- ifelse(crashes_variables$Surface.Conditions == "", "Unknown", crashes_variables$Surface.Conditions)
crashes_variables$Weather.Conditions <- ifelse(crashes_variables$Weather.Conditions == "", "Unknown", crashes_variables$Weather.Conditions)
crashes_variables$DRIVERGEN <- ifelse(is.na(crashes_variables$DRIVERGEN), "Unknown", crashes_variables$DRIVERGEN)
crashes_variables$DL_STATE <- ifelse(crashes_variables$DL_STATE == "", "Unknown", crashes_variables$DL_STATE)
crashes_variables$DL_STATE <- ifelse(is.na(crashes_variables$DL_STATE), "Unknown", crashes_variables$DL_STATE)

crashes_variables$CHARGED <- ifelse(is.na(crashes_variables$CHARGED), 0, crashes_variables$CHARGED)
crashes_variables$ALCRESULT <- ifelse(is.na(crashes_variables$ALCRESULT), 0, crashes_variables$ALCRESULT)
crashes_variables$DRUGRESULT <- ifelse(is.na(crashes_variables$DRUGRESULT), 77, crashes_variables$DRUGRESULT)
crashes_variables$MAKE <- ifelse(crashes_variables$MAKE == "", "Unknown", crashes_variables$MAKE)
crashes_variables$MAKE <- ifelse(is.na(crashes_variables$MAKE), "Unknown", crashes_variables$MAKE)
crashes_variables$MODEL <- ifelse(crashes_variables$MODEL == "", "Unknown", crashes_variables$MODEL)
crashes_variables$MODEL <- ifelse(is.na(crashes_variables$MODEL), "Unknown", crashes_variables$MODEL)
crashes_variables$SPEEDLIMIT <- ifelse(is.na(crashes_variables$SPEEDLIMIT), "Unknown", crashes_variables$SPEEDLIMIT)
crashes_variables <- crashes_variables[!is.na(crashes_variables$DRIVERAGE), ]


crashes_variables$Year <- as.factor(crashes_variables$Year)
crashes_variables$CHARGED <- as.factor(crashes_variables$CHARGED)
crashes_variables$DRUGRESULT <- as.factor(crashes_variables$DRUGRESULT)
crashes_variables$SPEEDLIMIT <- as.factor(crashes_variables$SPEEDLIMIT)
crashes_variables$Crash.Severity <- as.factor(crashes_variables$Crash.Severity)


crashes_variables$weather <- case_when(
  crashes_variables$Weather.Conditions %in% c("Snow", "Sleet, hail", "Blowing snow") |
    crashes_variables$Surface.Conditions %in% c("Snow", "Slush") ~ "Snow",
  crashes_variables$Weather.Conditions %in% c("Freezing rain/drizzle") |
    crashes_variables$Surface.Conditions %in% c("Ice/frost") ~ "Icy",
  crashes_variables$Weather.Conditions %in% c("Rain") | 
    crashes_variables$Surface.Conditions %in% c("Wet", "Water (standing or moving)") ~ "Rain",
  crashes_variables$Weather.Conditions %in% c("Fog, smoke, smog", "Blowing sand, soil, dirt") ~ "No visibility",
  crashes_variables$Weather.Conditions %in% c("Cloudy") ~ "Cloudy",
  crashes_variables$Weather.Conditions %in% c("Clear") | 
    crashes_variables$Surface.Conditions %in% c("Dry") ~ "Clear",
  TRUE ~ "Other"
)

crashes_severity <- crashes_variables %>% select(-c("Crash.Severity","Surface.Conditions", "Weather.Conditions", "Number.of.Injuries","Number.of.Major.Injuries", "Number.of.Minor.Injuries", "Number.of.Possible.Injuries", "Number.of.Unknown.Injuries"))

crash_severity <- crashes_variables %>% select("Crash.Severity")

library(ordinal)

rf.weather = randomForest(Crash.Severity~., data = crashes_variables, mtry = 6, importance = TRUE, ntree = 100, keep.inbag=TRUE)


library(glmnet)
grid = 10^seq(10,-2,length=100)

x <- model.matrix(Amount.of.Property.Damage ~ . - 1, data = crashes_variables)

cv.out.lasso = cv.glmnet(x,crashes_variables$Amount.of.Property.Damage,alpha = 1, lambda = grid) 
#default performs 10-fold CV, but you can change this using the argument `nfolds` 
plot(cv.out.lasso)
bestlambda2 = cv.out.lasso$lambda.min
bestlambda2

lasso.train = glmnet(x[train,],Y[train],alpha=1,lambda=grid)

which(grid==bestlambda2)
lasso.train$lambda[77]
coef(lasso.train)[,77]

lasso.pred = predict(lasso.train,s=bestlambda2,newx=x[test,])
mean((lasso.pred-Y.test)^2)

final.lasso = glmnet(x,Y,alpha=1,lambda=bestlambda2)
coef(final.lasso)

