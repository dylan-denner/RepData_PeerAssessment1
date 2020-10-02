---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, sep = ",") 

data$Date <- as.POSIXct(data$date, format="%Y-%m-%d")
```
## What is mean total number of steps taken per day?


```r
daily_steps <- aggregate(data$steps ~ data$Date, FUN = sum, )
colnames(daily_steps) <- c("Date", "Steps")

mean_steps_per_day <- mean(daily_steps$Steps, na.rm = TRUE)
median_steps_per_day <- median(daily_steps$Steps, na.rm = TRUE)

hist(daily_steps$Steps, breaks = 10, ylab = "Frequency", xlab = "Steps", main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?

```r
library(ggplot2)
steps_per_interval <- aggregate(data$steps ~ data$interval, FUN = mean, )
colnames(steps_per_interval) <- c("Interval", "AverageSteps")

p <- ggplot(steps_per_interval, aes(x=Interval, y=AverageSteps)) +
  geom_line() +
  xlab("")
p
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
most_active <- steps_per_interval[which.max(steps_per_interval$AverageSteps),]

most_active
```

```
##     Interval AverageSteps
## 104      835     206.1698
```
## Imputing missing values

```r
new_steps_df <- data

new_steps_df[is.na(new_steps_df)] = mean(steps_per_interval$AverageSteps)

new_daily_steps <- aggregate(new_steps_df$steps ~ new_steps_df$Date, FUN = sum, )

colnames(new_daily_steps) <- c("Date", "Steps")

mean_steps_per_day <- mean(new_daily_steps$Steps, na.rm = TRUE)
median_steps_per_day <- median(new_daily_steps$Steps, na.rm = TRUE)

hist(new_daily_steps$Steps, breaks = 10, ylab = "Frequency", xlab = "Steps", main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
new_steps_df$day <- weekdays(as.Date(new_steps_df$date))

weekday_df <- subset(new_steps_df, new_steps_df$day != "Saturday" &  new_steps_df$day != "Sunday")

weekend_df <- subset(new_steps_df, new_steps_df$day == "Saturday" |  new_steps_df$day == "Sunday")


weekday_steps_per_interval <- aggregate(weekday_df$steps ~ weekday_df$interval, FUN = mean, )

weekend_steps_per_interval <- aggregate(weekend_df$steps ~ weekend_df$interval, FUN = mean, )


colnames(weekday_steps_per_interval) <- c("Interval", "WeekdayAverageSteps")

colnames(weekend_steps_per_interval) <- c("Interval", "WeekdendAverageSteps")

q <- ggplot(weekday_steps_per_interval, aes(x=Interval, y=WeekdayAverageSteps)) +
  geom_line() +
  xlab("")
q
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
e <- ggplot(weekend_steps_per_interval, aes(x=Interval, y=WeekdendAverageSteps)) +
  geom_line() +
  xlab("")
e
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->
