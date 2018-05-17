# clean
rm(list=ls())

getwd()

bike_train <- read.csv("bike_train.csv", sep = ",")

bike_train$datetime <- as.POSIXct(bike_train$datetime, tz = "", format = "%Y-%m-%d%H:%M:%S")

bike_train$season <- factor(bike_train$season, labels = c("Winter", "Spring", "Summer", "Fall"))
bike_train$holidy <- factor(bike_train$holidy, labels = c("Not_Holiday", "Holiday"))
bike_train$workingday <- factor(bike_train$workingday, labels = c("Not_Working_Day", "Working_Day"))
bike_train$weather <- factor(bike_train$weather, labels = c(c("Good", "Normal", "Bad", "Very Bad"))


