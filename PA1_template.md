# Reproducible Research: Peer Assessment 1


## 0. Loading and preprocessing the data

```r
# reading the file
act_data <- read.csv("activity.csv")
# open libraries
library(plyr)
library(ggplot2)
library(scales)
# pre-processing
# total steps per day
steps_day <- ddply(act_data, .(date), summarise, sum.steps = sum(steps, na.rm = TRUE))
# mean steps per five minute interval across all days
steps_int <- ddply(act_data, .(interval), summarise, mean_steps = mean(steps, na.rm = TRUE))
```


## 1. What is mean total number of steps taken per day?
### 1.1 Make a histogram of the total number of steps taken each day.


```r
act_data$date <- as.Date(act_data$date)
act_data$steps <- as.double(act_data$steps)
qplot(act_data$date, y=act_data$steps, geom="histogram", binwidth = 61, stat="identity",
      xlab = "Date", ylab = "Total steps per day", 
      main = "Total Number Of Steps Taken Each Day")
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

### 1.2 Calculate the mean and median total number of steps taken per day


```r
mean_steps_day <- mean(steps_day$sum.steps, na.rm = TRUE)
median_steps_day <- median(steps_day$sum.steps, na.rm = TRUE)
```

The mean of the total number of steps per day is 9354.2295. And the median of the total number of steps per day is 10395.

## 2. What is the average daily activity pattern?
### 2.1 A time series plot (type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days.


```r
# plot of average steps per interval
plot(steps_int$interval, steps_int$mean_steps, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "Five minute intervals in 24 hours of the day",
     ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

### 2.2 Which 5-minute interval contains the maximum number of steps?

Which 5-minute interval, on average across all the days (y-axis) in the dataset, contains the maximum number of steps?

```r
max_int <- steps_int$interval[which.max(steps_int$mean_steps)]
max_step_int <- steps_int$mean_steps[which.max(steps_int$mean_steps)]
```

So interval 835 has the highest average number of steps amounting to 206.1698.


## 3. Imputing missing values



## 4. Are there differences in activity patterns between weekdays and weekends?
