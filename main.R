# clean
rm(list=ls())
getwd()

# load data
bike_train <- read.csv("bike_train.csv", sep = ",")

# format datetime column
bike_train$datetime <- as.POSIXct(bike_train$datetime, tz = "UTC", format = "%Y-%m-%d%H:%M:%S")

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
boxplot(bike_train$count~bike_train$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# create factored day column from datetime
install.packages("lubridate", dependencies = TRUE)
library(lubridate)
bike_train$day=wday(as.Date(bike_train$datetime,"%Y-%m-%d%H:%M:%S"), label=TRUE, locale=)
boxplot(bike_train$count~bike_train$day,xlab="day", ylab="count of rentals", col=rainbow(length(unique(bike_train$day))),outline=FALSE)

par(mfrow=c(2,1));
# boxplot relation between hour and count of rentals in workdays
bike_train_filtered = bike_train[bike_train$workingday == 1, ];
boxplot(main="Relation between hour and count of rentals in workdays", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# boxplot relation between hour and count of rentals in non-workdays
bike_train_filtered = bike_train[bike_train$workingday == 0, ];
boxplot(main="Relation between hour and count of rentals in non-workdays", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

par(mfrow=c(2,2));
# boxplot relation between hour and count of rentals in winter
bike_train_filtered = bike_train[bike_train$season == 1, ];
boxplot(main="Relation between hour and count of rentals in winter", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# boxplot relation between hour and count of rentals in spring
bike_train_filtered = bike_train[bike_train$season == 2, ];
boxplot(main="Relation between hour and count of rentals in spring", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# boxplot relation between hour and count of rentals in summer
bike_train_filtered = bike_train[bike_train$season == 3 ];
boxplot(main="Relation between hour and count of rentals in summer", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# boxplot relation between hour and count of rentals in fall
bike_train_filtered = bike_train[bike_train$season == 4, ];
boxplot(main="Relation between hour and count of rentals in fall", bike_train_filtered$count~bike_train_filtered$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)

# boxplot relation between hour and count of rentals in non-workdays
boxplot(bike_train$count~bike_train$hour,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour))),outline=FALSE)




library(ggplot2)

ggplot(data = bike_train, aes(temp,count)) + geom_point(alpha = 0.3, aes(color = temp)) + theme_bw()

# plot relation between count and date, with points colored according to temp
bike_train$datetime <- as.POSIXct(bike_train$datetime)
pl <- ggplot(bike_train,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)
pl + ggtitle("Relation between count and date, with points colored according to temperature") + scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') + theme_bw()

cor(bike_train[,c('temp','count')])

ggplot(bike_train,aes(factor(season),count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()



bikes_train_lm <- lm(data = bike_train, count ~ temp, xlab="Temperature", ylab="Count")
summary(bikes_train_lm)
plot(bike_train$temp, bike_train$count, pch = 20, cex = .5, col = "blue", main = "Count plotted against temperature", xlab = "Temperature", ylab = "Count")
abline(lm(data = bike_train, count ~ temp))


## 70% of the sample size
smp_size <- floor(0.7 * nrow(bike_train))
set.seed(4242)
train_ind <- sample(seq_len(nrow(bike_train)), size = smp_size)

subset_train <- bike_train[train_ind, ]
subset_test <- bike_train[-train_ind, ]

