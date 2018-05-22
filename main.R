# installs 
# install.packages("lubridate", dependencies = TRUE)
# install.packages("ggplot", dependencies = TRUE)
#install.packages("plyr", dependencies = TRUE)
#library(ggplot2)
#library(lubridate)
#library(randomForest)

# clean
rm(list=ls())
getwd()

# load data
bike_train <- read.csv("bike_train.csv", sep = ",")
bike_test <- read.csv("bike_test.csv", sep = ",")

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
bike_train$season_factored <- factor(bike_train$season, labels = c("Winter", "Spring", "Summer", "Fall"))
bike_train$holidy <- factor(bike_train$holiday, labels = c("Not_Holiday", "Holiday"))
bike_train$workingday_factored <- factor(bike_train$workingday, labels = c("Not_Working_Day", "Working_Day"))
bike_train$weather_factored <- factor(bike_train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))

# split up datetime into components
bike_train$month <- as.integer(format(as.POSIXlt(bike_train$datetime), format = "%m"))
bike_train$weekday <- as.integer(format(as.POSIXlt(bike_train$datetime), format = "%u"))
bike_train$day_name=wday(as.Date(bike_train$datetime,"%Y-%m-%d%H:%M:%S", tz = "UTC"), label=TRUE)
bike_train$hour <- as.integer(format(as.POSIXlt(bike_train$datetime), format = "%H"))
bike_train$hour_factored <- as.factor(bike_train$hour)

# boxplot relation between hour and count of rentals
boxplot(bike_train$count~bike_train$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)
boxplot(bike_train$count~bike_train$day_name,xlab="day", ylab="count of rentals", col=rainbow(length(unique(bike_train$day))),outline=FALSE)

par(mfrow=c(2,1));
# boxplot relation between hour and count of rentals in workdays
bike_train_filtered = bike_train[bike_train$workingday == 1, ];
boxplot(main="Relation between hour and count of rentals in workdays", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

# boxplot relation between hour and count of rentals in non-workdays
bike_train_filtered = bike_train[bike_train$workingday == 0, ];
boxplot(main="Relation between hour and count of rentals in non-workdays", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

par(mfrow=c(2,2));
# boxplot relation between hour and count of rentals in winter
bike_train_filtered = bike_train[bike_train$season == 1, ];
boxplot(main="Relation between hour and count of rentals in winter", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

# boxplot relation between hour and count of rentals in spring
bike_train_filtered = bike_train[bike_train$season == 2, ];
boxplot(main="Relation between hour and count of rentals in spring", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

# boxplot relation between hour and count of rentals in summer
bike_train_filtered = bike_train[bike_train$season == 3, ];
boxplot(main="Relation between hour and count of rentals in summer", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

# boxplot relation between hour and count of rentals in fall
bike_train_filtered = bike_train[bike_train$season == 4, ];
boxplot(main="Relation between hour and count of rentals in fall", bike_train_filtered$count~bike_train_filtered$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)

# boxplot relation between hour and count of rentals in non-workdays
boxplot(bike_train$count~bike_train$hour_factored,xlab="hour", ylab="count of rentals", col=rainbow(length(unique(bike_train$hour_factored))),outline=FALSE)


# plot relation between count and date, with points colored according to temp

bike_train$datetime <- as.POSIXct(bike_train$datetime)
pl <- ggplot(bike_train,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)
pl + ggtitle("Relation between count and date, with points colored according to temperature") + scale_color_continuous(low = '#55D8CE',high = '#FF6E2E') + theme_bw()



# section 2
bikes_train_lm <- lm(data = bike_train, count ~ temp)
summary(bikes_train_lm)
plot(bike_train$temp, bike_train$count, pch = 20, cex = .5, col = "blue", main = "Count plotted against temperature", xlab = "Temperature", ylab = "Count")
abline(lm(data = bike_train, count ~ temp))


## 70% of the sample size
smp_size <- floor(0.7 * nrow(bike_train))
set.seed(4242)
train_ind <- sample(seq_len(nrow(bike_train)), size = smp_size)

subset_train <- bike_train[train_ind, ]
subset_test <- bike_train[-train_ind, ]

subset_train_lm <- lm(data = subset_train, count ~ temp + hour)
summary(subset_train_lm)

subset_train_lm_factored <- lm(data = subset_train, count ~ temp + hour_factored)
summary(subset_train_lm_factored)

# create hour windows in datasets
bike_train$hour_window<-NA
bike_train$hour_window<-ifelse(bike_train$hour>=0 & bike_train$hour<=6 | bike_train$hour==23,"night", bike_train$hour_window)
bike_train$hour_window<-ifelse(bike_train$hour>=7 & bike_train$hour<=9,"morning commute", bike_train$hour_window)
bike_train$hour_window<-ifelse(bike_train$hour>=10 & bike_train$hour<=15,"midday", bike_train$hour_window)
bike_train$hour_window<-ifelse(bike_train$hour>=16 & bike_train$hour<=19,"evening commute", bike_train$hour_window)
bike_train$hour_window<-ifelse(bike_train$hour>=20 & bike_train$hour<=22,"late evening", bike_train$hour_window)
bike_train$hour_window <- as.factor(bike_train$hour_window)


# create hour windows in training
subset_train$hour_window<-NA
subset_train$hour_window<-ifelse(subset_train$hour>=0 & subset_train$hour<=6 | subset_train$hour==23,"night", subset_train$hour_window)
subset_train$hour_window<-ifelse(subset_train$hour>=7 & subset_train$hour<=9,"morning commute", subset_train$hour_window)
subset_train$hour_window<-ifelse(subset_train$hour>=10 & subset_train$hour<=15,"midday", subset_train$hour_window)
subset_train$hour_window<-ifelse(subset_train$hour>=16 & subset_train$hour<=19,"evening commute", subset_train$hour_window)
subset_train$hour_window<-ifelse(subset_train$hour>=20 & subset_train$hour<=22,"late evening", subset_train$hour_window)
subset_train$hour_window <- as.factor(subset_train$hour_window)

# create hour windows in test
subset_test$hour_window<-NA
subset_test$hour_window<-ifelse(subset_test$hour>=0 & subset_test$hour<=6 | subset_test$hour==23,"night", subset_test$hour_window)
subset_test$hour_window<-ifelse(subset_test$hour>=7 & subset_test$hour<=9,"morning commute", subset_test$hour_window)
subset_test$hour_window<-ifelse(subset_test$hour>=10 & subset_test$hour<=15,"midday", subset_test$hour_window)
subset_test$hour_window<-ifelse(subset_test$hour>=16 & subset_test$hour<=19,"evening commute", subset_test$hour_window)
subset_test$hour_window<-ifelse(subset_test$hour>=20 & subset_test$hour<=22,"late evening", subset_test$hour_window)
subset_test$hour_window <- as.factor(subset_test$hour_window)


# set midday as the reference level
subset_train$hour_window=relevel(subset_train$hour_window, "midday")

# linear regression
subset_train.lm <- lm(data = subset_train, count ~ temp + hour_window)
summary(subset_train.lm)

subset_train.im <- lm(data = subset_train, count ~ temp*hour_window)
summary(subset_train.im)


subset_test$predictTest = predict(subset_train.lm, subset_test)
lm.sse_test = sum((subset_test$count - subset_test$predictTest)^2)
lm.sst_test = sum((subset_test$count - mean(bike_train$count))^2)
1 - lm.sse_test/lm.sst_test

subset_test$predictTest2 = predict(subset_train.im, subset_test)
im.sse_test = sum((subset_test$count - subset_test$predictTest2)^2)
im.sst_test = sum((subset_test$count - mean(bike_train$count))^2)
1 - im.sse_test/im.sst_test


# Section 3
cor(bike_train$count,bike_train$humidity)
cor(bike_train$count,bike_train$atemp)
cor(bike_train$count,bike_train$temp)

# plot count as function of atemp, to find optimal atemp
plot(bike_train$atemp, bike_train$count, type = 'h', col= 'blue', xlab = 'Actual Temperature', ylab = 'Total Bike Rentals')

# normalize atemp in the datasets
bike_train$atemp_normalized<-NA
bike_train$atemp_normalized<-ifelse(bike_train$atemp>32, 60+(46-bike_train$atemp)*40/14, bike_train$atemp_normalized);
bike_train$atemp_normalized<-ifelse(bike_train$atemp<=32, bike_train$atemp*100/32, bike_train$atemp_normalized);
subset_test$atemp_normalized<-NA
subset_test$atemp_normalized<-ifelse(subset_test$atemp>32, 60+(46-subset_test$atemp)*40/14, subset_test$atemp_normalized);
subset_test$atemp_normalized<-ifelse(subset_test$atemp<=32, subset_test$atemp*100/32, subset_test$atemp_normalized);

# create agg_temp variable in datasets
bike_train$agg_temp = NA
bike_train$agg_temp = (bike_train$atemp_normalized - bike_train$humidity)*4 - bike_train$weather*10 + bike_train$season*25
subset_test$agg_temp = NA
subset_test$agg_temp = (subset_test$atemp_normalized - subset_test$humidity)*4 - subset_test$weather*10 + subset_test$season*25

# see correlation of agg_temp to count
cor(bike_train$count, bike_train$agg_temp)

bike_train.final <- lm(data = bike_train, count ~ holiday*hour_window+workingday_factored*hour_window+hour*hour_window+agg_temp*hour_window)
summary(bike_train.final)

subset_test$predictTest = predict(bike_train.final, subset_test)
final.sse_test = sum((subset_test$count - subset_test$predictTest)^2)
final.sst_test = sum((subset_test$count - mean(bike_train$count))^2)
1 - final.sse_test/final.sst_test


