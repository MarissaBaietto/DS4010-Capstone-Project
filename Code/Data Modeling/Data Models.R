library(readr)
Cleaned_Data_1 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_2 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_3 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_4 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_5 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_6 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_7 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_8 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_9 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_10 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_11 <- read.csv(file.choose(), header = TRUE)
Cleaned_Data_12 <- read.csv(file.choose(), header = TRUE)

Unique_Crashes <- rbind(Cleaned_Data_1, Cleaned_Data_2, Cleaned_Data_3, Cleaned_Data_4, Cleaned_Data_5, Cleaned_Data_6, Cleaned_Data_7, Cleaned_Data_8, Cleaned_Data_9, Cleaned_Data_10,
                        Cleaned_Data_11, Cleaned_Data_12)

Unique_Crashes <- Unique_Crashes %>% filter(Year != 2025)

crashes_variables <- Unique_Crashes %>% select(-c("X.1","X","X.2","Iowa.DOT.Case.Number","Date.of.Crash","Time.of.Crash","Number.of.Fatalities","Location","HasVehicleData", "City.Name", "Fatal"))


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

Model_Data <- crashes_variables

##--Crash Severity----
library(glmnet)
library(randomForest)
library(dplyr)
library(forcats)

Model_Data[] <- lapply(Model_Data, function(x) {
  if (is.character(x)) factor(x) else x
})

Model_Data$DRUGRESULT <- as.factor(Model_Data$DRUGRESULT)

Model_Data$Bodily_Harm <- as.factor(ifelse(Model_Data$Crash.Severity %in% c("Fatal", "Major Injury", "Minor Injury"), 1, 0))

Before_Crash <- Model_Data %>% select(c("Bodily_Harm","Month.of.Crash","Day.of.Week","Hour","Drug.or.Alcohol","Environmental.Conditions","Light.Conditions","Surface.Conditions","Weather.Conditions","Work.Zone","DRIVERAGE","DRIVERGEN","ALCRESULT","DRUGRESULT","SPEEDLIMIT", "MAKE","MODEL"))

sample_index = sample(1 : 498681, size = 50000, replace = F)

sample <- Before_Crash[sample_index,]

sample$MODEL <- fct_lump(sample$MODEL, n = 20)

sample$MAKE <- fct_lump(sample$MAKE, n = 20)

sample$Bodily_Harm <- ifelse(sample$Bodily_Harm == 1,1,0)
sample$Month.of.Crash <- relevel(sample$Month.of.Crash, ref = "Jan")
sample$Day.of.Week <- relevel(sample$Day.of.Week, ref = "Mon")
sample$Drug.or.Alcohol <- relevel(sample$Drug.or.Alcohol, ref = "None Indicated")
sample$Environmental.Conditions <- relevel(sample$Environmental.Conditions, ref = "None apparent")
sample$Light.Conditions <- relevel(sample$Light.Conditions, ref = "Daylight")
sample$Weather.Conditions <- relevel(sample$Weather.Conditions, ref = "Clear")

sample <- sample %>% select(-c("MAKE","MODEL"))

glm_severity <- glm(Bodily_Harm~., data=sample)

library(broom)
tidy(glm_severity, exponentiate = TRUE) %>%
  filter(p.value < 0.05)


amount_to_add <- min((Model_Data %>% filter(Amount.of.Property.Damage > 0))$Amount.of.Property.Damage)

Model_Data_2 <- Model_Data %>% mutate(Amount.of.Property.Damage = log(Amount.of.Property.Damage + amount_to_add)) %>% 
  select(c("Amount.of.Property.Damage","Month.of.Crash","Day.of.Week","Hour","Drug.or.Alcohol","Environmental.Conditions","Light.Conditions","Surface.Conditions","Weather.Conditions","Work.Zone","DRIVERAGE","DRIVERGEN","ALCRESULT","DRUGRESULT","SPEEDLIMIT"))

sample <- Model_Data_2[sample_index,]

model_property <- lm(Amount.of.Property.Damage~., data = sample)

save(model_property, file = "Property_Damage_Model.RData")

































































































var_imp <- as.data.frame(rf$importance[,c(0,1)])
var_imp$variable <- rownames(var_imp)
colnames(var_imp) <- c("importance", "variable")

# Sort and plot top 15
library(ggplot2)

ggplot(var_imp[order(-var_imp$importance)[1:15], ], aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Important Variables for Crash Severity",
       x = "Variable", y = "Importance") +
  theme_minimal()


library(iml)
library(dplyr)



# Prepare the data
X <- sample %>% select(-Bodily_Harm)
y <- sample$Bodily_Harm

#predictor <- Predictor$new(rf, data = X, y = y, type = "prob")
rf$response_var <- factor(rf$response_var, levels = c(0, 1), labels = c("No Bodily Harm", "Bodily Harm"))
set.seed(123)
sample_idx <- sample(1:nrow(X), 1000)  # or even 500
predictor_small <- Predictor$new(rf, data = X[sample_idx, ], y = y[sample_idx], type = "prob")

pdp_plot <- FeatureEffect$new(predictor_small, feature = "Hour", method = "pdp")
pdp_plot$plot() + 
  scale_y_continuous(
    breaks = c(0, 0.9),  # Adjust y-axis scale if necessary
    labels = c("No Bodily Harm", "Bodily Harm")
  ) +
  theme_minimal() + 
  labs(
    y = "Predicted Severity",
    title = "Impact of Hour of Day on Crash Severity",
    subtitle = "Prediction for 'No Bodily Harm' vs. 'Bodily Harm'"
  )


# PDP for one variable
pdp_plot <- FeatureEffect$new(predictor, feature = "Hour", method = "pdp")
pdp_plot$plot()









# Convert all character columns to factors
Model_Data[] <- lapply(Model_Data, function(x) {
  if (is.character(x)) factor(x) else x
})

Model_Data$DRUGRESULT <- as.factor(Model_Data$DRUGRESULT)

Model_Data$Bodily_Harm <- as.factor(ifelse(Model_Data$Crash.Severity %in% c("Fatal", "Major Injury", "Minor Injury"), 1, 0))

sample_index = sample(1 : 498681, size = 50000, replace = F)
 
Before_Crash <- Model_Data %>% select(c("Bodily_Harm","Month.of.Crash","Day.of.Week","Hour","Drug.or.Alcohol","Environmental.Conditions","Light.Conditions","Surface.Conditions","Weather.Conditions","Work.Zone","DRIVERAGE","DRIVERGEN","ALCRESULT","DRUGRESULT","SPEEDLIMIT"))

sample <- Before_Crash[sample_index,]

rf <- randomForest(Bodily_Harm~., data=Before_Crash, mtry=3, ntree=100, importance=TRUE, keep.inbag=TRUE)

varImpPlot(rf)

table(Before_Crash$Bodily_Harm,predicted)
predicted <- ifelse(bodily_harm_model$fitted.values > 0.3,1,0)

bodily_harm_model <- glm(Bodily_Harm~., data=Before_Crash,family='binomial')

save(bodily_harm_model, file = "Bodily_Harm_Model.RData")

severity_model <- glm(Bodily_Harm~Amount.of.Property.Damage + SPEEDLIMIT + Environmental.Conditions + Light.Conditions + Drug.or.Alcohol + DRIVERGEN, data=Model_Data,family='binomial')
severity_model2 <- glm(Bodily_Harm~Amount.of.Property.Damage + First.Harmful.Event + SPEEDLIMIT + Environmental.Conditions + Light.Conditions + Drug.or.Alcohol + Manner.of.Crash.Collision, data=Model_Data,family='binomial')
severity_model3 <- glm(Bodily_Harm~CHARGED + Amount.of.Property.Damage + First.Harmful.Event + SPEEDLIMIT + Environmental.Conditions + Light.Conditions + Drug.or.Alcohol + DRIVERGEN + Manner.of.Crash.Collision, data=Model_Data,family='binomial')
bodily_harm_model <- glm(Bodily_Harm~DRUGRESULT + CHARGED + Amount.of.Property.Damage + First.Harmful.Event + SPEEDLIMIT + Environmental.Conditions + Light.Conditions + Drug.or.Alcohol + DRIVERGEN + Manner.of.Crash.Collision, data=Model_Data,family='binomial')

save(severity_model, file = "Crash_Severity_Model.RData")

load("Crash_Severity_Model.RData")

predicted_values <- ifelse(severity_model$fitted.values <= 0.25, 0, 1)

table(Model_Data$Bodily_Harm, predicted_values)

##--Amount of Property Damage----
library(glmnet)
library(ISLR2)
library(leaps)

grid = 10^seq(10,-2,length=100)

Model_Data_2 <- Model_Data %>% select(-c("X", "weather", "DRUGRESULT", "MODEL", "MAKE", "DL_STATE", "County.Name","First.Harmful.Event","Major.Cause"))

amount_to_add <- min((Model_Data_2 %>% filter(Amount.of.Property.Damage > 0))$Amount.of.Property.Damage)

Model_Data_2 <- Model_Data %>% mutate(Amount.of.Property.Damage = log(Amount.of.Property.Damage + amount_to_add))

sample_index = sample(1 : 498681, size = 1000, replace = F)

Model_Data[] <- lapply(Model_Data, function(x) {
  if (is.character(x)) factor(x) else x
})

sample_2 <- Model_Data_2[sample_index,]

regfit.fwd = regsubsets(Amount.of.Property.Damage~.,data=sample_2, nvmax=9, method="forward") 

property_damage <- lm(Amount.of.Property.Damage ~ DRIVERGEN + Number.of.Injuries + Number.of.Vehicles.Involved + DRIVERAGE + SPEEDLIMIT, data = Model_Data_2)
save(property_damage, file = "Property_Damage_Model.RData")

x <- model.matrix(Amount.of.Property.Damage ~ ., data = sample_2)

cv.out.lasso = cv.glmnet(x,sample_2$Amount.of.Property.Damage,alpha = 1, lambda = grid) 

bestlambda2 = cv.out.lasso$lambda.min

lasso.pred = predict(lasso.train,s=bestlambda2,newx=x[test,])

final.lasso = glmnet(x,sample_2$Amount.of.Property.Damage,alpha=1,lambda=bestlambda2)
coef(final.lasso)

