library(dplyr)
library(knitr)
library(ggplot2)
library(httpuv)
library(lattice)

##Read the File
activity <- read.csv("activity.csv")

##Remove NA lines (This created more work later!!!!)
##activity <- na.omit(NAactivity)

##Read dates as dates, including days of the week
activity$date <- as.Date(activity$date)
activity <- data.frame(steps = activity$steps, interval = activity$interval,
                       date = activity$date, day_of_week = tolower(weekdays(activity$date)))
activity <- cbind(activity, 
                  type_of_day = ifelse(activity$day_of_week == "saturday" |
                                               activity$day_of_week == "sunday", "weekend", "weekday"))

##Compute total number of steps by day NA Removed
dailysteps <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(dailysteps) <- c("date", "steps")

##Generatehistogram depicting total steps taken each day NA Removed
hist(dailysteps$steps, xlab = "Total Number of Steps", ylab = "Frequency", 
     col = "skyblue", border = "black",
     main = "Total Steps Taken Each Day")

##Display mean and median daily steps
cat("The mean daily steps are", mean(dailysteps$steps))
cat("The median daily steps are", median(dailysteps$steps))

##Compute mean by day NA Removed
meansteps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(meansteps) <- c("interval", "mean_steps")

##Generate the time series NA Removed
plot(meansteps$interval, meansteps$mean_steps, type = "l", col = "skyblue",
     xlab = "Interval in Minutes", ylab = "Average Number of Steps",
     main = "Average Total Steps Taken Each Day")

##Discover the 5-minute interval with the highest steps
maxmeansteps <- which(meansteps$mean_steps == max(meansteps$mean_steps))
maxinterval <- meansteps[maxmeansteps, 1]
cat("The interval", maxinterval, "has the maximum average steps of", max(meansteps$mean_steps))

##Identifying & Dealing with NAs
cat("The total number of NAs is", sum(is.na(activity)))
NAlocation <- which(is.na(activity$steps))
NAmean_vector <- rep(mean(activity$steps, na.rm = TRUE, times = length(NAlocation)))
activity[NAlocation, "steps"] <- NAmean_vector

##Compute total number of steps by day NA Present
dailysteps <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = FALSE)
names(dailysteps) <- c("date", "steps")

##Generatehistogram depicting total steps taken each day NA Present
hist(dailysteps$steps, xlab = "Total Number of Steps", ylab = "Frequency", 
     col = "skyblue", border = "black",
     main = "Total Steps Taken Each Day with NAs")

##Display mean and median daily steps
cat("The mean daily steps are", mean(dailysteps$steps))
cat("The median daily steps are", median(dailysteps$steps))

##Compute mean by day NA Present
meansteps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = FALSE)
names(meansteps) <- c("interval", "mean_steps")

##Generate the time series NA Present
plot(meansteps$interval, meansteps$mean_steps, type = "l", col = "skyblue",
     xlab = "Interval in Minutes", ylab = "Average Number of Steps",
     main = "Average Total Steps Taken Each Day with NAs")

##Generate the weekday vs weekend graphs
meanstepsbyday <- aggregate(activity$steps, by = list(activity$type_of_day, activity$day_of_week, activity$interval), mean)
names(meanstepsbyday) <- c("type_of_day", "day_of_week", "interval", "mean")
xyplot(mean ~ interval | type_of_day, meanstepsbyday, type = "l", 
       xlab = "Interval", ylab = "Number of Steps", 
       main =  "Average Steps Taken by Day Interval with NAs", layout = c(1,2))


