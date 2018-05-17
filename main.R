# clean
rm(list=ls())

getwd()

bike_train <- read.csv("bike_train.csv", sep = ",")

bike_train$datetime <- as.POSIXct(bike_train$datetime, tz = "", format = "%Y-%m-%d%H:%M:%S")

bike_train$season <- factor(bike_train$season, labels = c("Winter", "Spring", "Summer", "Fall"))

