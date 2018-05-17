# clean
rm(list=ls())

getwd()

bike_train <- read.csv("bike_train.csv", sep = ",")

bike_train$datetime <- as.POSIXct(bike_train$datetime, tz = "", format = "%Y-%m-%d%H:%M:%S")

par(mfrow=c(4,2));
par(mar = rep(2, 4));
hist(bike_train$humidity);
hist(bike_train$temp);
hist(bike_train$atemp);
hist(bike_train$windspeed);
hist(bike_train$season);
hist(bike_train$weather);
hist(bike_train$holiday);
hist(bike_train$workingday);

bike_train$season <- factor(bike_train$season, labels = c("Winter", "Spring", "Summer", "Fall"))
bike_train$holidy <- factor(bike_train$holiday, labels = c("Not_Holiday", "Holiday"))
bike_train$workingday <- factor(bike_train$workingday, labels = c("Not_Working_Day", "Working_Day"))
bike_train$weather <- factor(bike_train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))




