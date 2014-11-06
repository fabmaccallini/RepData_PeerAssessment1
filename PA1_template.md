# Reproducible Research: Peer Assessment 1
  
## Loading and preprocessing the data

```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "repdata-data-activity.zip", mode = "wb")
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```
  
## What is mean total number of steps taken per day?

```r
daily.steps <- tapply(data$steps, data$date, sum)
hist(daily.steps, main = "Total nuber of steps per day", xlab = "Date", ylab = "Total number of steps")
mean <- mean(daily.steps, na.rm = TRUE)
mean.string <- as.character(round(mean, digits = 2))
median <- median(daily.steps, na.rm = TRUE)
abline(v = mean, col = "red")
legend("topright", "mean", lty = c(1, 1), col = "red")
```

![](./PA1_template_files/figure-html/exploredata-1.png) 
The average number of steps taken per day is 10766.19 (red line) and the median is 10765.  
  
## What is the average daily activity pattern?

```r
int.steps <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
plot(as.numeric(names(int.steps)), int.steps, type = 'l', main = "Average number of steps per daily interval", xlab = "Interval", ylab = "Avera steps")
max <- max(int.steps)
max.string <- as.character(round(max, digits = 2))
int <- names(int.steps[which(int.steps == max)])
abline(v = int, col = "blue")
```

![](./PA1_template_files/figure-html/pattern-1.png) 
The daily observations show a peak at the interval 835 (blue line) with a maximum of 206.17 average number of steps.  
  
## Imputing missing values

```r
NA.count <- sum(is.na(data$steps))
data.adj <- data
missing.days <- sum(is.na(daily.steps))
for (i in 1: length(daily.steps)) {
    if (is.na(daily.steps[i])) {
        # substitute all NAs with the average value of the interval
        data.adj$steps[data.adj$date == names(daily.steps)[i]] <- int.steps
    }
}
```
The number of missing data is 2304. All missing are in data have been replaced by the average number of steps of the interval they belonged to.

```r
daily.steps.adj <- tapply(data.adj$steps, data.adj$date, sum)
hist(daily.steps.adj, main = "Total nuber of steps per day", xlab = "Date", ylab = "Total number of steps")
```

![](./PA1_template_files/figure-html/imputdata2-1.png) 

```r
mean.adj <- mean(daily.steps.adj)
mean.adj.string <- as.character(round(mean.adj, digits = 2))
median.adj <- median(daily.steps.adj)
median.adj.string <- as.character(round(median.adj, digits = 2))
```
The new average number of steps per day is 10766.19 and the new median is 10766.19. The mean has not changed, as we expected, however the central bin in new histogram increased by 8. The reason is that all missing data belonged to 8 specific days having a total number of steps equal to the mean and the mean falls in the central bin. The median has suffered a small increase because it falls in one of the days having missing data and is equal to the mean (by construction).  

## Are there differences in activity patterns between weekdays and weekends?

```r
data.adj$week <- weekdays(data.adj$date)
data.adj$week[data.adj$week == "Saturday" | data.adj$week == "Sunday"] <- "weekend"
data.adj$week[data.adj$week != "weekend"] <- "weekday"
int.steps2 <- data.frame(with(data.adj, tapply(steps, list(interval, week), mean)))
with(int.steps2, {
    par(mfrow = c(2, 1))
    plot(rownames(int.steps2), int.steps2[, 1], type = "l", main = colnames(int.steps2)[1], xlab = "Interval", ylab = "Number of teps")
    plot(rownames(int.steps2), int.steps2[, 2], type = "l", main = colnames(int.steps2)[2], xlab = "Interval", ylab = "Number of teps")
})
```

![](./PA1_template_files/figure-html/weekdays-1.png) 
