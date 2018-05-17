# clean
rm(list=ls())
getwd()

# load data
bike_train <- read.csv("bike_train.csv", sep = ",")

# format datetime column
bike_train$datetime <- as.POSIXct(bike_train$datetime, tz = "", format = "%Y-%m-%d%H:%M:%S")

# initial histograms for overview of data
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

# factorize non-continuous columns
bike_train$season <- factor(bike_train$season, labels = c("Winter", "Spring", "Summer", "Fall"))
bike_train$holidy <- factor(bike_train$holiday, labels = c("Not_Holiday", "Holiday"))
bike_train$workingday <- factor(bike_train$workingday, labels = c("Not_Working_Day", "Working_Day"))
bike_train$weather <- factor(bike_train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))

# create factored hour column from datetime
bike_train$hour=substr(bike_train$datetime,12,13)
bike_train$hour=as.factor(bike_train$hour)

# boxplot relation between hour and count of rentals
boxplot(bike_train$count~bike_train$hour,xlab="hour", ylab="count of rentals")

library(ggplot2)

ggplot(data = bike_train, aes(temp,count)) + geom_point(alpha = 0.3, aes(color = temp)) + theme_bw()

bike_train$datetime <- as.POSIXct(bike_train$datetime)
pl <- ggplot(bike_train,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)
pl + scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') + theme_bw()

cor(bike_train[,c('temp','count')])

ggplot(bike_train,aes(factor(season),count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()

