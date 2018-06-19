---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

##Loading and preprocessing the data

Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


```r
library( data.table)

activity_NA <- data.table( read.csv( "activity.csv", colClasses = c( "integer", "character", "integer")))

activity_NA$date <- as.Date( activity_NA$date)

activity <- na.omit(activity_NA)
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

Calculate and report the mean and median of the total number of steps taken per day


```r
aggdt <- aggregate( steps~date, activity, sum)

head( aggdt )
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
hist( aggdt$steps, col = "green", xlab  = "Total Steps",
      ylab = "Frequency", main = "Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
print( paste( "mean = ",  mean( aggdt$steps, na.rm = TRUE)))
```

```
## [1] "mean =  10766.1886792453"
```

```r
print( paste( "median = ",  median( aggdt$steps, na.rm = TRUE)))
```

```
## [1] "median =  10765"
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
aggdt1 <- aggregate( steps~interval, activity, mean)

with( aggdt1, plot( interval, steps,  type = "l" , ylab = "average number of steps", xlab = "interval", main = "Average number of steps across all days"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
aggdt1[ which.max( aggdt1$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
####hence the 835th interval on average across all the days in the dataset, contains the maximum number of steps.

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
sum( is.na(activity_NA$steps) | is.na( activity_NA$date) | is.na( activity_NA$interval))
```

```
## [1] 2304
```
I picked the strategy of replacing NA’s with the mean for that 5-minute interval.


```r
activity_no_NA <- activity_NA
for( i in 1:nrow(activity_no_NA)){
        if( is.na( activity_no_NA$steps[i])){
                activity_no_NA$steps[i] <- aggdt1$steps[   activity_no_NA$interval[i] == aggdt1$interval]
        }
}

aggdt2 <- aggregate( steps~date, activity_no_NA, sum)

head( aggdt2 )
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist( aggdt2$steps, col = "orange", xlab  = "Total Steps",
      ylab = "Frequency", main = "Steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
print( paste( "mean = ",  mean( aggdt2$steps, na.rm = TRUE)))
```

```
## [1] "mean =  10766.1886792453"
```

```r
print( paste( "median = ",  median( aggdt2$steps, na.rm = TRUE)))
```

```
## [1] "median =  10766.1886792453"
```
####hence the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs) is 2304

####after the NAs have been replaced by mean for that 5-minute interval,Mean values stays the same but therer is slight difference in meadian value.

#Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
week.end <- function(x){
        y <- weekdays( x)
        if( y == "Sunday" | y == "Saturday"){
                return( TRUE)
        }
        else{ return( FALSE)}
}

activity_no_NA$weekend <- sapply( activity_no_NA$date, week.end)

weekday_data <- activity_no_NA[ !activity_no_NA$weekend]

weekend_data <- activity_no_NA[ activity_no_NA$weekend]

par( mfrow = c( 1,2), oma = c(0, 0, 2, 0))

aggdt3 <- aggregate( steps~interval, weekday_data, mean)
aggdt4 <- aggregate( steps~interval, weekend_data, mean)

with( aggdt3, plot( interval, steps,  type = "l" , ylab = "average number of steps", xlab = "interval", main = "week days" , ylim = c(0,300)))

with( aggdt4, plot( interval, steps,  type = "l" , ylab = "average number of steps", xlab = "interval", main = "week ends", ylim = c(0,300)))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
