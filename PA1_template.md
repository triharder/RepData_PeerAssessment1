# Reproducible Research: Peer Assessment 1
triharder  
Started August 5, 2015 due August 16, 2015  


### Loading and preprocessing the data

```r
#1 Load the data
raw <- read.csv("activity.csv")
#2 Process/transform
proc <- na.omit(raw)
```

The mean and median number of steps taken per day are calculated as follows:
### What is mean total number of steps taken per day?

```r
#1 calculate total steps per day
sum_steps = sapply(split(proc$steps, proc$date), sum)

#2 histogram of total steps per day
hist(sum_steps, main="Total Number of Steps Per Day (Missing Values Removed)", xlab = "Steps", ylab = "Frequency", ylim=range(0:30))
```

![](Figs/stats-1.png) 
The mean and median number of steps taken per day are calculated as follows:

```r
#3 mean and median total number of steps taken per day
avg_spd <- mean(sum_steps)
med_spd <- median(sum_steps)
```
Respectively, the mean and median number of steps taken per day are **9354 and 10395**.

### What is the average daily activity pattern?

```r
#1 Time series plot of 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg_spi <- tapply(proc$steps, proc$interval, mean)
plot(unique(proc$interval),avg_spi,type="l",main="Average Steps Per Interval Across All Days",xlab="5-minute Interval",ylab="Steps",xlim = c(0,2400),xaxt="n")
axis(side=1,at=c(0,600,1200,1800,2400))
#2 5-minute interval with max steps across all days
avg_spi <- as.data.frame.table(avg_spi)
colnames(avg_spi)<-c("int","avgSPI")
max_int <- avg_spi$int[which.max(avg_spi$avgSPI)]
abline(v=835,lty=3,col="red")
axis(side=1,at=835)
```

![](Figs/avg_daily_activity-1.png) 
The maximum number of steps on average (206) occurs during the **835** interval, as shown by red dotted line above.

### Imputing missing values
There are **2304** missing values in the raw data, determined by the following expression:

```r
#1 Calculate total missing values
nrow(raw)-nrow(proc)
```
Implementing the strategy to fill missing values based on the average for relative interval, both the process and new dataset can be observed below:

```r
#2 Process to fill in missing values
##  Fill NA values in 'steps' with the value for respective interval averaged across all days.

#3 New dataset replacing missing values
fill <- raw
fill$steps[is.na(fill$steps)]<-avg_spi$avgSPI

#4 Histogram
sum_steps_fill = sapply(split(fill$steps, fill$date), sum,na.rm=T)
hist(sum_steps_fill, main="Total Number of Steps Per Day (Missing Values Replaced)", xlab = "Steps", ylab = "Frequency", ylim=range(0:40))
```

![](Figs/imputing-1.png) 

```r
avg_spd_fill <- mean(sum_steps_fill)
med_spd_fill <- median(sum_steps_fill)
```
After imputing the data, there are **0** **NAs**.  

The mean and median number of steps taken per day are **10766 and 10766**, respectively.

These values differ only slightly from the estimates from the first part of the assignment.

The impact of imputing missing data on the estimates of the total daily number of steps reduces variablity.  Overall, there is no added value and less clarity.


### Are there differences in activity patterns between weekdays and weekends?

```r
#1 Create factor variable with "weekday" and "weekend" levels
#use weekdays() on filled data
library(dplyr)
library(lubridate)
fill <- mutate(fill,Weekday = wday(ymd(fill$date),label=T))
fill <- mutate(fill,Weekday = factor(ifelse(Weekday %in% c("Sat","Sun"),"Weekend","Weekday")))

#2 Panel plot with time series averaged per level (Weekday or Weekend)
library(ggplot2)
avg_spw <- aggregate(steps ~ interval + Weekday,fill,mean)
ggplot(avg_spw,aes(interval,steps)) + geom_line() + labs(x="Interval",y="Average Steps") + facet_wrap(~Weekday, nrow=2, ncol =1)
```

![](Figs/day_pattern-1.png) 
