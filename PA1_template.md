---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## 1. Loading and preprocessing the data

Set the working directory , download the data and unzip into the folder
```
setwd("D:/OneDrive/Public/Data Science/Course 5/RepData_PeerAssessment1")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "dataset.zip"
download.file(url, destfile)
unzip(destfile)
```

Read the csv file

```
activity <- read.csv("activity.csv", sep = ",")
```

The variable names and the structureand get the names of an object

```
names(activity)

```

```
## [1] "steps"    "date"     "interval"

```

```
str(activity)

```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
```
summary(activity)
```
```
head(activity[which(!is.na(activity$steps)), ])
```

## 2. Mean of "total number of step taken per day" over all days

Group the number of steps by date and interval

```
library(reshape2)
activity_melt <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))
head(activity_melt)
steps_sum <- dcast(activity_melt, date ~ variable, sum)
head(steps_sum)
```

Total number of steps per day
```
summary(steps_sum$steps) # Summary of data
```

Histogram of total number of steps taken sans NA rows. Also, showing mean and median of the data.

```
hist(steps_sum$steps, main = "Histogram of total steps taken per day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "steel blue")
abline(v = mean(steps_sum$steps), lty = 1, lwd = 2, col = "red")
abline(v = median(steps_sum$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("red", "black"), 
       lty = c(1, 2), lwd = c(2, 2))
```

## 3. What is the average daily activity pattern?

Make a time series plot

```
stepsmeaninterval <- dcast(activity_melt, interval ~ variable, 
                           mean, na.rm = TRUE)
head(stepsmeaninterval)
plot(stepsmeaninterval$interval, stepsmeaninterval$steps, ty = "l",
     xlab = "time interval", ylab = "Average steps", 
     main = "Average steps taken over all days vs \n time interval")
```
Time interval during which the maximum number of steps

```
maxsteps_interval <- 
        stepsmeaninterval$interval[which.max(stepsmeaninterval$steps)]
maxsteps_interval
```

## 4. Imputing missing values

Replace the missing data for a day by the time average over all other days

```
activity2 <- split(activity, activity$interval)
activity2 <- lapply(activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL

activity2 <- split(activity2, activity2$date)
df <- lapply(activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL
head(activity2)
```

Refutes the intervals being disjoint
```
library(reshape2)
activity_melt2 <- melt(activity2, id.vars = c("date", "interval"))
steps_sum <- dcast(activity_melt2, date ~ variable, sum, na.rm = TRUE)
head(steps_sum)
```

Histogram of total number of steps taken with imputed missing values
```
hist(steps_sum$steps, main = "Histogram of total steps taken per day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "steel blue")
abline(v = mean(steps_sum$steps), lty = 1, lwd = 2, col = "red")
abline(v = median(steps_sum$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("red", "black"), 
       lty = c(2, 1), lwd = c(2, 2))
```

Number of rows with NA values
```
sum(is.na(activity$steps))
sum(is.na(activity$steps))*100/nrow(activity) # Percentage of rows
```

## 5. Are there differences in activity patterns between weekdays and weekends?

Create a new column for the date is a weekday or weekend.
```
library(lubridate)
weekends <- which(weekdays(as.Date(activity2$date)) == "Saturday" |
                          weekdays(as.Date(activity2$date)) == "Sunday")
weekdays <- which(weekdays(as.Date(activity2$date)) != "Saturday" &
                          weekdays(as.Date(activity2$date)) != "Sunday")
temp <- c(rep("a", length(activity2)))
temp[weekends] <- "weekend"
temp[weekdays] <- "weekday"
length(temp)
activity2 <- cbind(activity2, temp)
head(activity2)
names(activity2)[4] <- "day"
```

Steps taken over each interval averaged across weekday days and weekend days
```
activity2split <- split(activity2, activity2$day)
stepsmean_interval <- lapply(activity2split, function(x) {
        temp <- aggregate(x$steps, list(x$interval), mean)
        names(temp) <- c("interval", "steps")
        return(temp)
})
```

Unsplit stepsmean_interval
```
stepsmean_interval <- do.call("rbind", stepsmean_interval)
weekdays <- grep("weekday" ,row.names(stepsmean_interval))
weekends <- grep("weekend" ,row.names(stepsmean_interval))
temp <- c(rep("a", length(stepsmean_interval$steps)))
temp[weekdays] <- "weekdays"
temp[weekends] <- "weekends"
names(temp) <- "day"
stepsmean_interval <- cbind(stepsmean_interval, temp)
row.names(stepsmean_interval) <- NULL
```

The mean number of steps taken over the weekdays and weekends.
```
head(stepsmean_interval)
```

Plot the graph
```
library(ggplot2)
ggplot(stepsmean_interval, aes(interval, steps)) + geom_line() + facet_grid(temp ~ .) 
```
