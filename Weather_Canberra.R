suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(corrplot))

#Reading CSV file
suppressPackageStartupMessages(library(caret))
set.seed(1023)
weather_data5 <- read.csv("weather_data5.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
colnames(weather_data5) 
head(weather_data5)
#Computing Training Set testing set
train_rec <- createDataPartition(weather_data5$RainTomorrow, p = 0.7, list = FALSE)
training_set <- weather_data5[train_rec,]
testing_set <- weather_data5[-train_rec,]

#9am Forecast model 1
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictors_9am_c1 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "Temp9am")
formula_9am_c1 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c1, collapse="+"), sep="~"))
mod9am_c1_fit <- train(formula_9am_c1,  data=training_set, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit <- train(formula_9am_c1,  data=training_set, method="rf", family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit <- train(formula_9am_c1,  data=training_set, method="", family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit$results$Accuracy

(summary_rep <- summary(mod9am_c1_fit$finalModel))
drop1(mod9am_c1_fit$finalModel, test="Chisq")

#9am Forecast model 2
predictors_9am_c2 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "MinTemp")
formula_9am_c2 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c2, collapse="+"), sep="~"))
mod9am_c2_fit <- train(formula_9am_c2,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c2_fit$results$Accuracy
(summary_rep <- summary(mod9am_c2_fit$finalModel))

library(caret)
mod9am_pred <- predict(mod9am_c1_fit, testing_set)
confusionMatrix(mod9am_pred, testing_set[,"RainTomorrow"])

#3Pm ForeCast model

predictors_3pm_c1 <- c("Cloud3pm", "Humidity3pm", "Pressure3pm", "Temp3pm")
formula_3pm_c1 <- as.formula(paste("RainTomorrow", paste(predictors_3pm_c1, collapse="+"), sep="~"))
mod3pm_c1_fit <- train(formula_3pm_c1,  data = training, method = "glm", family = "binomial",trControl = trControl, metric = 'Accuracy')
mod3pm_c1$fitresults$Accuracy
(summary_rep <- summary(mod3pm_c1_fit$finalModel))

drop1(mod3pm_c1_fit$finalModel, test="Chisq")

mod3pm_pred <- predict(mod3pm_c1_fit, testing)
confusionMatrix(mod3pm_pred, testing[,"RainTomorrow"])

#Evening Forecast model

predictors_evening_c1 <- c("Pressure3pm", "Temp3pm", "Sunshine")
formula_evening_c1 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c1, collapse="+"), sep="~"))
mod_ev_c1_fit <- train(formula_evening_c1,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c1_fit$results$Accuracy

(summary_rep <- summary(mod_ev_c1_fit$finalModel))
drop1(mod_ev_c1_fit$finalModel, test="Chisq")

#Evening Forecast model 2
predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir", "WindGustSpeed")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy
(summary_rep <- summary(mod_ev_c2_fit$finalModel))
drop1(mod_ev_c2_fit$finalModel, test="Chisq")

#WindGustDir has some borderline p-value for some specific directions. 
#WindGustDir is not significative and we should drop it from the model. 
#Hence, we redefine such model after having taken WindGustDir off.

predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy

(summary_rep <- summary(mod_ev_c2_fit$finalModel))
drop1(mod_ev_c2_fit$finalModel, test="Chisq")

#WindGustDirESE is reported as significant and hence including WindGustDir was a right choice and accept that model as a candidate one.

#Evening Forecast model 3
predictors_evening_c3 <- c("Pressure3pm", "Sunshine")
formula_evening_c3 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c3, collapse="+"), sep="~"))
mod_ev_c3_fit <- train(formula_evening_c3,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c3_fit$results$Accuracy
(summary_rep <- summary(mod_ev_c3_fit$finalModel))
drop1(mod_ev_c3_fit$finalModel, test="Chisq")

#ANOVA Analysis

anova(mod_ev_c2_fit$finalModel, mod_ev_c3_fit$finalModel, test="Chisq")

#Based on p-value, there is no significative difference between them. 
#We then choose both models. 
#The first model because it provides with a better accuracy then the second. 
#The second model for its simplicity. Let us evaluate the test accuracy for both of them.
library(caret)
modevening_pred <- predict(mod_ev_c2_fit, testing_set)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])

modevening_pred <- predict(mod_ev_c3_fit, testing_set)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])


#Saving the final models

saveRDS(list(weather_data5, train_rec, training_set, testing_set, mod9am_c1_fit, mod9am_c2_fit, mod3pm_c1_fit, mod_ev_c2_fit, mod_ev_c3_fit), file="wf_log_reg_part2.rds")

glm.tune(mod9am_c1_fit, training_set)

all.equal(weather_data$Rainfall > 1, weather_data$RainToday == "Yes")

#Deleting any NA if there
weather_data6 <- subset(weather_data, select = -c(Date, Location, RISK_MM, RainToday, WindDir9am, WindDir3pm))
weather_data6$RainfallTomorrow <- c(weather_data6$Rainfall[2:nrow(weather_data6)], NA)
weather_data6$Humidity3pmTomorrow <- c(weather_data6$Humidity3pm[2:nrow(weather_data6)], NA)
weather_data6$WindGustSpeedTomorrow <- c(weather_data6$WindGustSpeed[2:nrow(weather_data6)], NA)
weather_data6$SunshineTomorrow <- c(weather_data6$Sunshine[2:nrow(weather_data6)], NA)
weather_data6$MinTempTomorrow <- c(weather_data6$MinTemp[2:nrow(weather_data6)], NA)
weather_data6$MaxTempTomorrow <- c(weather_data6$MaxTemp[2:nrow(weather_data6)], NA)

weather_data7 = weather_data6[complete.cases(weather_data6),]
head(weather_data7)
#Chances_OF_Rain_Function
chance_of_rain <- function(model, data_record)
  {
    chance_frac <- predict(mod_ev_c3_fit, data_record, type="prob")[, "Yes"]
    paste(round(chance_frac*100), "%", sep="")
}
#Tomorrow's_RainFall_Prediction :
weather_data8 = weather_data7[weather_data7$RainfallTomorrow > 1,]
rf_fit <- lm(RainfallTomorrow ~  MaxTemp + Sunshine + WindGustSpeed - 1, data = weather_data8)
summary(rf_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(rf_fit, weather_data8)
plot(x = seq_along(weather_data8$RainfallTomorrow), y = weather_data8$RainfallTomorrow, type='p', xlab = "observations", ylab = "RainfallTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data8$RainfallTomorrow), y = lm_pred, col='red')

#Tomorrow's_Humidity_3PM_Prediction
h3pm_fit <- lm(Humidity3pmTomorrow ~ Humidity3pm + Sunshine, data = weather_data7)
summary(h3pm_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(h3pm_fit, weather_data7)
plot(x = seq_along(weather_data7$Humidity3pmTomorrow), y = weather_data7$Humidity3pmTomorrow, type='p', xlab = "observations", ylab = "Humidity3pmTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$Humidity3pmTomorrow), y = lm_pred, col='red')

#Tomorrow's_WindGust_Prediction
wgs_fit <- lm(WindGustSpeedTomorrow ~ WindGustSpeed + Pressure9am + Pressure3pm, data = weather_data7)
summary(wgs_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(wgs_fit, weather_data7)
plot(x = seq_along(weather_data7$WindGustSpeedTomorrow), y = weather_data7$WindGustSpeedTomorrow, type='p', xlab = "observations", ylab = "WindGustSpeedTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$WindGustSpeedTomorrow), y = lm_pred, col='red')

#Tomorrow's_SunShine_Prediction
sun_fit <- lm(SunshineTomorrow ~ Sunshine*Humidity3pm + Cloud3pm + Evaporation + I(Evaporation^2) + WindGustSpeed - 1, data = weather_data7)
summary(sun_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(sun_fit, weather_data7)
plot(x = seq_along(weather_data7$SunshineTomorrow), y = weather_data7$SunshineTomorrow, type='p', xlab = "observations", ylab = "SunshineTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$SunshineTomorrow), y = lm_pred, col='red')

#Tomorrow's_Min_Temp_Prediction
minTemp_fit <- lm(MinTempTomorrow ~ MinTemp + Humidity3pm , data = weather_data7)
summary(minTemp_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(minTemp_fit, weather_data7)
plot(x = weather_data7$Sunshine, y = weather_data7$MinTemp, type='p', xlab = "Sunshine", ylab = "MinTemp")
legend("topright", c("actual", "fitted"), fill = c("black", "green"))
points(x = weather_data7$Sunshine, y = lm_pred, col='green')

#Tomorrow's_Max_temp_Prediction
maxTemp_fit <- lm(MaxTempTomorrow ~ MaxTemp + Evaporation, data = weather_data7)
summary(maxTemp_fit)

#All Figures are Significantly Reported Plotting
lm_pred <- predict(maxTemp_fit, weather_data7)
plot(x = weather_data7$Sunshine, y = weather_data7$MaxTemp, type='p', xlab = "Sunshine", ylab = "MaxTemp")
legend("topright", c("actual", "fitted"), fill = c("black", "red"))
points(x = weather_data7$Sunshine, y = lm_pred, col='red')

#Cloud_Data_Computation
computeCloudConditions = function(cloud_9am, cloud_3pm) {
  cloud_avg = min(round((cloud_9am + cloud_3pm)/2), 8)
 
  if (cloud_avg == 8) {
    cc_str = "Cloudy"
  } else if (cloud_avg >= 6) {
    cc_str = "Mostly Cloudy"
  } else if (cloud_avg >= 3) {
    cc_str = "Partly Cloudy"
  } else if (cloud_avg >= 1) {
    cc_str = "Mostly Sunny"
  } else if (cloud_avg < 1) {
    cc_str = "Sunny"
  }
  cc_str
}

#if RainTomorrow == "Yes" Rainfall is explicitely given, otherwise un upper bound Rainfall < 1 mm is. 
#Chance of rain is computed only if RainTomorrow prediction is "Yes". 
#The Humidity3pm prediction is taken as humidity prediction for the whole day, in general.

#Calculation_Of_Weather_Report_Function_Definition
weather_report <- function(today_record, rain_tomorrow_model, cutoff) {
  # RainTomorrow  prediction
  rainTomorrow_prob <- predict(rain_tomorrow_model, today_record, type="prob")
  rainTomorrow_pred = ifelse(rainTomorrow_prob$Yes >= cutoff, "Yes", "No")
  
  # Rainfall prediction iff RainTomorrow prediction is Yes; chance of rain probability
  rainfall_pred <- NA
  chance_of_rain <- NA
  if (rainTomorrow_pred == "Yes") {
    rainfall_pred <- round(predict(rf_fit, today_record), 1)
    chance_of_rain <- round(rainTomorrow_prob$Yes*100)
  }
  
  # WindGustSpeed prediction
  wgs_pred <- round(predict(wgs_fit, today_record), 1)
  
  # Humidity3pm prediction
  h3pm_pred <- round(predict(h3pm_fit, today_record), 1)
  
  # sunshine prediction is used to fit Cloud9am and Cloud3pm
  sun_pred <- predict(sun_fit, today_record)
  
  cloud9am_pred <- min(round(predict(cloud9am_fit, data.frame(Sunshine=sun_pred))), 8)
  cloud3pm_pred <- min(round(predict(cloud3pm_fit, data.frame(Sunshine=sun_pred))), 8)
  # a descriptive cloud conditions string is computed
  CloudConditions_pred <- computeCloudConditions(cloud9am_pred, cloud3pm_pred)
  
  # MinTemp prediction
  minTemp_pred <- round(predict(minTemp_fit, today_record), 1)
  
  # MaxTemp prediction
  maxTemp_pred <- round(predict(maxTemp_fit, today_record), 1)
  
  # converting all numeric predictions to strings
  if (is.na(rainfall_pred)) {
    rainfall_pred_str <- "< 1 mm"
  } else {
    rainfall_pred_str <- paste(rainfall_pred, "mm", sep = " ")
  }
  
  if (is.na(chance_of_rain)) {
    chance_of_rain_str <- ""
  } else {
    chance_of_rain_str <- paste(chance_of_rain, "%", sep="")
  }
  
  wgs_pred_str <- paste(wgs_pred, "Km/h", sep= " ")
  h3pm_pred_str <- paste(h3pm_pred, "%", sep = "")
  minTemp_pred_str <- paste(minTemp_pred, "?C", sep= "")
  maxTemp_pred_str <- paste(maxTemp_pred, "?C", sep= "")
  
  report <- data.frame(Rainfall = rainfall_pred_str,
                       ChanceOfRain = chance_of_rain_str,
                       WindGustSpeed = wgs_pred_str, 
                       Humidity = h3pm_pred_str,
                       CloudConditions = CloudConditions_pred,
                       MinTemp = minTemp_pred_str,
                       MaxTemp = maxTemp_pred_str)
  report
  
  sample<-c("Rainfall","ChanceOfRain","WindGustSpeed","Humidity","CloudConditions","MinTemp","MaxTemp")
  return(sample)

  }

#Weather_Forcast_Testing_Function_is_Working_or_not
(tomorrow_report <- weather_report(weather_data5[282,],mod9am_c1_fit, 0.56))



