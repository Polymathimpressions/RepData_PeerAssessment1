---
title: "Reproducible Research Project 1"
author: "Keith Krause"
date: "10/11/2019"
output: 
  html_document: 
    keep_md: yes
---



# 1. Loading and Processing the Activity Data

## Load Packages

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```
## Load Activity Data from Working Directory

```r
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
```
## Explore the activity data

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## Process and transform data

```r
activity$date<-ymd(activity$date)
```
# 2. Histogram of Total Number of Steps Taken per Day
## Caluculate total steps per day and plot histogram

```r
stepsperday<-tapply(activity$steps, activity$date, sum)
hist(stepsperday, xlab="Number of Steps", main = "Steps per Day")
```

![](PA1_template_files/figure-html/steps per day-1.png)<!-- -->

# 3. Calculate mean and median of Total Number of Steps per Day

```r
meanperday<- mean(stepsperday, na.rm = TRUE)
medianperday<- median(stepsperday, na.rm=TRUE)
```
Mean= 10766.19
Median=10765

# 4. Time Series Plot of Average Number of Steps

```r
stepsperinterval<- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(as.numeric(names(stepsperinterval)), 
     stepsperinterval,
     xlab = "Interval",
     ylab = "Steps", 
     main = "Average Number of Steps per Interval",
     type = "l")
```

![](PA1_template_files/figure-html/Average Steps-1.png)<!-- -->



# 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
maxinterval<- names(sort(stepsperinterval, decreasing = TRUE)[1])
maxsteps<- sort(stepsperinterval, decreasing = TRUE)[1]
```
The 5-minute interval with the maximum number of steps is 835 with 206.1698 steps

# 6. Code to desctibe and show a strategy for imputing missing data

```r
numberNA<- sum(is.na(activity$steps))
```
Missing Values (NA) in the data set are 2304

## Strategy for filling in all of the missing values in the data set
I will fill in the missing values (NA) in the activity data set by calculating the mean number of steps across all of the days that have data for that interval.  A new data set will be created called clean which will be used to better estimate a complete set of data.  

```r
#split by interval
activitysplit<- split(activity, activity$interval)
#fill in missing data
for(i in 1:length(activitysplit)){
  activitysplit[[i]]$steps[is.na(activitysplit[[i]]$steps)]<- stepsperinterval[i]
}
clean<- do.call("rbind", activitysplit)
clean<- clean[order(clean$date),]
```
# 7. Histogram of Total Number of Steps Taken per day after missing values are imputed

```r
stepsperdayclean<-tapply(clean$steps, clean$date, sum)
hist(stepsperdayclean, xlab="Number of Steps", main="Steps per Day (Imputed Data)")
```

![](PA1_template_files/figure-html/Plot Imputed Total Steps-1.png)<!-- -->
## Mean and Median of Total Number of Steps Taken per day with Imputed data

```r
meanstepsperdayclean<- mean(stepsperdayclean,na.rm=TRUE)
medianstepsperdayclean<- median(stepsperdayclean, na.rm = TRUE)
```
The mean for the imputed data remained the same at 10766.19.
The median for the imputed data is 10766.19 which increased by 1.19 from the original data's median calculation(10765)

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## Loop to create day of the week column to classify weekend or weekday

```r
for(i in 1:nrow(clean)) {
  if (weekdays(clean$date [i]) =="Saturday" | weekdays(clean$date[i])=="Sunday"){
    clean$dayofweek[i]= "weekend"
  }else {clean$dayofweek[i]="weekday"
  } 
}
```
## Calculate average steps per interval for weekend and weekdays

```r
weekendsteps<- tapply(clean[clean$dayofweek == "weekend",]$steps, clean[clean$dayofweek == "weekend",]$interval, mean, na.rm=TRUE)
weekdaysteps<- tapply(clean[clean$dayofweek == "weekday",]$steps, clean[clean$dayofweek == "weekday",]$interval, mean, na.rm=TRUE)
```
## Pannel Plot comparing Average Number of Steps Across Weekdays and Weekends

```r
par(mfrow=c(2,1))
#Weekday plot
plot(as.numeric(names(weekdaysteps)),
     weekdaysteps,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern (Weekdays)",
     type = "l")
#Weekend Plot
plot(as.numeric(names(weekendsteps)),
     weekendsteps,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern (Weekends)",
     type = "l")
```

![](PA1_template_files/figure-html/Plot Avg Steps Weekend/Weekday-1.png)<!-- -->
