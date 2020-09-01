---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE)
```


## Loading and preprocessing the data

```r
#1
data<- read.csv(unzip("./activity.zip" ))
#2
library(dplyr)
data<-as_tibble(data)
data$date <- as.factor(as.Date(data$date))
data
```

```
## # A tibble: 17,568 x 3
##    steps date       interval
##    <int> <fct>         <int>
##  1    NA 2012-10-01        0
##  2    NA 2012-10-01        5
##  3    NA 2012-10-01       10
##  4    NA 2012-10-01       15
##  5    NA 2012-10-01       20
##  6    NA 2012-10-01       25
##  7    NA 2012-10-01       30
##  8    NA 2012-10-01       35
##  9    NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

## What is mean total number of steps taken per day?


```r
#1. Calculate the total number of steps taken per day
data2<-aggregate(steps ~ date, data, sum)

#2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

hist(data2$steps, breaks = 25,
     main="steps taken each day",
     xlab = "Steps Mean") 
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

```r
#3. Calculate and report the mean and median of the total number of steps taken per day

summary(data2$steps,)[c("Median","Mean")]
```

```
##   Median     Mean 
## 10765.00 10766.19
```

## What is the average daily activity pattern?


```r
#1. Make a time series plot type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
data3<-tapply(data$steps,data$interval,mean,na.rm=T)
plot(row.names(data3),data3,type ="l",
     main = "average daily activity pattern",
     xlab = "Intervals",
     ylab = "Steps Mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
subset(data3, data3==max(data3))
```

```
##      835 
## 206.1698
```



## Imputing missing values

```r
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(data))
```

```
## [1] 2304
```

```r
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
library(data.table)
data3<- round(data3,digits = 0)
data4<-data.table(data3,names(data3))
colnames(data4)<- c("steps.mean","interval")
data4<-merge(data,data4,by="interval")

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
data4$steps[is.na(data4$steps)]<-data4$steps.mean[is.na(data4$steps)]
data4<- data4[,-4] #deletel the steps.mean column

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
data4.1<-aggregate(steps ~ date, data4, sum)
hist(data4.1$steps,breaks = 25,
     main="steps taken each day, with NA filled",
     xlab = "Steps Mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(data4.1$steps,)[c("Median","Mean")]
```

```
##   Median     Mean 
## 10762.00 10765.64
```

## Are there differences in activity patterns between weekdays and weekends?


```r
#1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data5<- data
data5$date<- as.Date(data5$date)
data5$weekday<- weekdays(data5$date)
data5$weekday<-sub("lunes|martes|miércoles|jueves|viernes","weekdays",data5$weekday)
data5$weekday<-sub("sábado|domingo" ,"weekend",data5$weekday)
data5$weekday<- as.factor(data5$weekday) 


#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(ggplot2)
data5.1<-aggregate(steps~interval + weekday,data=data5, FUN=mean)
s<-ggplot(data5.1,aes(interval,steps)) + geom_line()
s+facet_grid(rows = vars(weekday))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
