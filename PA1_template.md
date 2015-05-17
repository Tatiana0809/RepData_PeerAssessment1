---
title: "Peer Assessment 1"
author: "Tatiana"
date: "Sunday, May 17, 2015"
output: html_document
---

#Assignment 1 for Reproducible research course

First step is to download the data and create the data frame for the analysis


```r
act_data<-read.csv("C://Users//????????????????????????//Downloads//activity.csv",header=TRUE,sep=",")
```

```
## Warning in file(file, "rt"): cannot open file 'C://
## Users//????????????????????????//Downloads//activity.csv': Invalid argument
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
act_data$date<-as.POSIXct(act_data$date, format="%Y-%m-%d")
```

```
## Error in as.POSIXct(act_data$date, format = "%Y-%m-%d"): object 'act_data' not found
```

Now we can analyze our data and answer the following questions:

1. What is mean total number of steps taken per day?

2. What is the average daily activity pattern?

3. Are there differences in activity patterns between weekdays and weekends?

**What is mean total number of steps taken per day?**


```r
dates<-data.frame(row=1:61,date=c(unique(act_data$date)))
```

```
## Error in unique(act_data$date): object 'act_data' not found
```

```r
steps<-by(act_data$steps,act_data$date,sum,na.rm=TRUE)
```

```
## Error in by(act_data$steps, act_data$date, sum, na.rm = TRUE): object 'act_data' not found
```

```r
stepsbydate<-data.frame(as.POSIXct(dates[,2], format="%Y-%m-%d"),c(steps))
```

```
## Error in as.POSIXct(dates[, 2], format = "%Y-%m-%d"): object 'dates' not found
```

```r
row.names(stepsbydate)<-NULL
```

```
## Error in row.names(stepsbydate) <- NULL: object 'stepsbydate' not found
```

```r
mean(stepsbydate[,2],na.rm=TRUE)
```

```
## Error in mean(stepsbydate[, 2], na.rm = TRUE): object 'stepsbydate' not found
```

```r
median(stepsbydate[,2],na.rm=TRUE)
```

```
## Error in median(stepsbydate[, 2], na.rm = TRUE): object 'stepsbydate' not found
```

Now we can plot a histogram of the total number of steps taken each day


```
## Error in hist(stepsbydate$c.steps, breaks = seq(0, 22000, 1000), col = "grey", : object 'stepsbydate' not found
```

**What is the average daily activity pattern?**

To establish that we need to calculate the mean of steps taken in every 5 minute interval and make a time series plot.


```
## Error in unique(act_data$interval): object 'act_data' not found
```

```
## Error in by(act_data$steps, act_data$interval, mean, na.rm = TRUE): object 'act_data' not found
```

```
## Error in data.frame(interval = interval[, 2], steps = c(steps)): object 'interval' not found
```

```
## Error in row.names(stepsbyinterval) <- NULL: object 'stepsbyinterval' not found
```

```
## Error in plot(x = stepsbyinterval$interval, y = stepsbyinterval$steps, : object 'stepsbyinterval' not found
```

We also need to know which interval comtains the maximum number of steps


```r
max(stepsbyinterval$steps,na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'stepsbyinterval' not found
```

```r
stepsbyinterval$interval[stepsbyinterval$steps == max(stepsbyinterval$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'stepsbyinterval' not found
```

Our data frame contains some missimg values (NA), which may affect the analysis.
First we need to find how many missing values we have.


```r
sum(is.na(act_data$steps))
```

```
## Error in eval(expr, envir, enclos): object 'act_data' not found
```

In the new data set missing values were filled in with the mean for that day.


```
## Error in transform(act_data, steps = ifelse(is.na(steps), ave(steps, interval, : object 'act_data' not found
```

To compare two data sets we calculated the mean and median for the new data set and created histogram of the total number of steps taken each day


```r
steps2<-by(act_data2$steps,act_data2$date,sum,na.rm=TRUE)
```

```
## Error in by(act_data2$steps, act_data2$date, sum, na.rm = TRUE): object 'act_data2' not found
```

```r
stepsbydate2<-data.frame(as.POSIXct(dates[,2], format="%Y-%m-%d"),c(steps2))
```

```
## Error in as.POSIXct(dates[, 2], format = "%Y-%m-%d"): object 'dates' not found
```

```r
row.names(stepsbydate2)<-NULL
```

```
## Error in row.names(stepsbydate2) <- NULL: object 'stepsbydate2' not found
```

```r
mean(stepsbydate2[,2],na.rm=TRUE)
```

```
## Error in mean(stepsbydate2[, 2], na.rm = TRUE): object 'stepsbydate2' not found
```

```r
median(stepsbydate2[,2],na.rm=TRUE)
```

```
## Error in median(stepsbydate2[, 2], na.rm = TRUE): object 'stepsbydate2' not found
```

```r
hist(stepsbydate2$c.steps,breaks = seq(0,22000,1000),col="grey",
main="Total number of steps taken each day (October-November 2012)",
xlab="Total number of steps")
```

```
## Error in hist(stepsbydate2$c.steps, breaks = seq(0, 22000, 1000), col = "grey", : object 'stepsbydate2' not found
```

**Are there differences in activity patterns between weekdays and weekends?**

First we need to add a column specifing whether day was week day of weekend

```r
list_of_days <- as.Date(stepsbydate2[,1])
```

```
## Error in as.Date(stepsbydate2[, 1]): object 'stepsbydate2' not found
```

```r
names_of_days <- weekdays(list_of_days)
```

```
## Error in weekdays(list_of_days): object 'list_of_days' not found
```

```r
act_data2$wday <- weekdays(as.Date(act_data2$date))
```

```
## Error in as.Date(act_data2$date): object 'act_data2' not found
```

```r
d <- ifelse(act_data2$wday %in% c('??????????????','??????????????????????'), 
            "weekend", "weekday")
```

```
## Error in match(x, table, nomatch = 0L): object 'act_data2' not found
```

```r
act_data2$wknd_or_not <- d
```

```
## Error in eval(expr, envir, enclos): object 'd' not found
```

Now we can divide the data frame on two: first one will contain information about weekdays and the second - about weekends


```r
act_data2weekday <- subset(act_data2, wknd_or_not == "weekday")
```

```
## Error in subset(act_data2, wknd_or_not == "weekday"): object 'act_data2' not found
```

```r
act_data2weekend <- subset(act_data2, wknd_or_not == "weekend")
```

```
## Error in subset(act_data2, wknd_or_not == "weekend"): object 'act_data2' not found
```

Next we can calculate the average number of steps taken for the 5-minute intervals and make the plot

```
## Error in unique(act_data2weekday$interval): object 'act_data2weekday' not found
```

```
## Error in by(act_data2weekday$steps, act_data2weekday$interval, mean, na.rm = TRUE): object 'act_data2weekday' not found
```

```
## Error in data.frame(interval = interval2[, 2], steps = c(steps3)): object 'interval2' not found
```

```
## Error in row.names(stepsbyinterval2) <- NULL: object 'stepsbyinterval2' not found
```

```
## Error in unique(act_data2weekend$interval): object 'act_data2weekend' not found
```

```
## Error in by(act_data2weekend$steps, act_data2weekend$interval, mean, na.rm = TRUE): object 'act_data2weekend' not found
```

```
## Error in data.frame(interval = interval3[, 2], steps = c(steps4)): object 'interval3' not found
```

```
## Error in row.names(stepsbyinterval3) <- NULL: object 'stepsbyinterval3' not found
```

```
## Error in plot(x = stepsbyinterval2$interval, y = stepsbyinterval2$steps, : object 'stepsbyinterval2' not found
```

```
## Error in plot(x = stepsbyinterval3$interval, y = stepsbyinterval3$steps, : object 'stepsbyinterval3' not found
```

