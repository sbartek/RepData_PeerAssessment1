---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Reproducible Research: Peer Assessment 1
========================================

First we load all packages needed:

```r
require("knitr")
require('xtable')
require("data.table")
require("ggplot2")
```

Next we assure that date format is in English. It is Linux
specific, so if you run it on Windows it could not work.

```r
Sys.setlocale("LC_TIME", 'en_US.UTF-8')
```

[1] "en_US.UTF-8"

## Loading and preprocessing the data


```r
dataDir <- "data"
if (!file.exists(dataDir)) {dir.create(dataDir)}
unzip("activity.zip", exdir=dataDir)
activity.df <- read.csv(paste0(dataDir, "/activity.csv"))
activty.head <- xtable(head(activity.df))
print(activty.head, type="html")
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Sun Feb 15 22:36:32 2015 -->
<table border=1>
<tr> <th>  </th> <th> steps </th> <th> date </th> <th> interval </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">   0 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">   5 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  10 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  15 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  20 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  25 </td> </tr>
   </table>


## What is mean total number of steps taken per day?

### Head of the table with total number of steps per day (we have removed rows with NA).

```r
activity.dt <- as.data.table(activity.df)
steps <- activity.dt[!is.na(steps),.(number=sum(steps)),by=date]
print(xtable(head(steps)), type="html")
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Sun Feb 15 22:36:32 2015 -->
<table border=1>
<tr> <th>  </th> <th> date </th> <th> number </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-02 </td> <td align="right"> 126 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-03 </td> <td align="right"> 11352 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-04 </td> <td align="right"> 12116 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-05 </td> <td align="right"> 13294 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-06 </td> <td align="right"> 15420 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-07 </td> <td align="right"> 11015 </td> </tr>
   </table>

### Histogram of the total number of steps taken each day.

```r
steps.mean <- mean(steps$number)
steps.median <- median(steps$number)

steps.gg1 <- ggplot(steps, aes(x=number))
steps.gg1+geom_histogram(binwidth = 850) + geom_vline(xintercept = steps.median, colour = "red")  
```

![plot of chunk histograma](figure/histograma-1.png) 

```r
steps.gg2 <- ggplot(steps, aes(x=factor(0),y=number))
steps.gg2 + geom_boxplot()+coord_flip()
```

![plot of chunk histograma](figure/histograma-2.png) 

### Mean and median of the total number of steps taken per day.

```r
steps.mean
```

```
## [1] 10766.19
```

```r
steps.median
```

```
## [1] 10765
```
Mean is 10766.19 and median 10765.


## What is the average daily activity pattern?

### Time series plot

Plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps2 <- activity.dt[,.(average=mean(steps, na.rm=TRUE),median=median(steps, na.rm=TRUE)),by=interval]

interval.max <- steps2[which.max(steps2[,average]),interval]
average.max <- steps2[which.max(steps2[,average]),average]

ggplot(steps2, aes(x=interval, y=average))+geom_line(colour="black")+
    geom_vline(xintercept=interval.max, colour="blue")+
        geom_hline(yintercept=average.max, colour="red")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

### 5-minute interval that contains the maximum number of steps

835

## Imputing missing values

### Total number of missing values in the dataset

```r
sum(activity.dt[,is.na(steps)])
```

```
## [1] 2304
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

We use **mean for 5-minute interval** for filling in all of the missing values in the dataset.



```r
#In order to clone we wrap by data.table
activity.nona <- data.table(activity.dt)

steps.average <- function(int) {
  steps2[interval == int,average]
}

activity.nona[,steps := ifelse(is.na(steps), steps.average(interval), steps)]
```

```
##            steps       date interval
##     1: 1.7169811 2012-10-01        0
##     2: 0.3396226 2012-10-01        5
##     3: 0.1320755 2012-10-01       10
##     4: 0.1509434 2012-10-01       15
##     5: 0.0754717 2012-10-01       20
##    ---                              
## 17564: 4.6981132 2012-11-30     2335
## 17565: 3.3018868 2012-11-30     2340
## 17566: 0.6415094 2012-11-30     2345
## 17567: 0.2264151 2012-11-30     2350
## 17568: 1.0754717 2012-11-30     2355
```

```r
steps.total.per.day <- activity.dt[,.(total=sum(steps, na.rm=TRUE)), by=date]
steps.total.per.day.nona <- activity.nona[,.(total=sum(steps)), by=date]
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
ggplot(steps.total.per.day.nona)+geom_histogram(aes(x=total),binwidth=1000)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#qplot(total, data=steps.total.per.day, binwidth=1000)

steps.average.per.day <- activity.nona[,.(average=mean(steps)), by=date]
steps.median.per.day <- activity.nona[,.(median=median(steps)), by=date]
steps.mean.nona <- mean(steps.total.per.day.nona$total)
steps.median.nona <- median(steps.total.per.day.nona$total)
```



After filling in NA , mean is 10766.19
and median is 10766.19.
Before mean wss 10766.19 and median 10765.

### Impact of imputing missing data


```r
ggplot()+ geom_line(data=steps.total.per.day,aes(x=as.Date(date),y=steps.total.per.day.nona$total,color='blue'), alpha=0.4, size=1)+
    geom_line(data=steps.total.per.day,aes(x=as.Date(date),y=total,color='red'), alpha=0.4, size=0.8)+
    xlab('Date')+ylab('Steps per day') + 
    scale_colour_manual(name = '', 
         values =c('blue'='blue','red'='red'), labels = c('NA removed','with NA'))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

## Are there differences in activity patterns between weekdays and weekends?


### Panel plot containing time series plots

```r
activity.nona[,day:=as.factor(
                   ifelse(
                       (weekdays(as.Date(date)) %in% c("Saturday","Sunday")),
                       "weekend","weekday"))
              ]
```

```
##            steps       date interval     day
##     1: 1.7169811 2012-10-01        0 weekday
##     2: 0.3396226 2012-10-01        5 weekday
##     3: 0.1320755 2012-10-01       10 weekday
##     4: 0.1509434 2012-10-01       15 weekday
##     5: 0.0754717 2012-10-01       20 weekday
##    ---                                      
## 17564: 4.6981132 2012-11-30     2335 weekday
## 17565: 3.3018868 2012-11-30     2340 weekday
## 17566: 0.6415094 2012-11-30     2345 weekday
## 17567: 0.2264151 2012-11-30     2350 weekday
## 17568: 1.0754717 2012-11-30     2355 weekday
```

```r
steps3 <- activity.nona[,.(average=mean(steps, na.rm=TRUE)),
                        by=.(interval, day)]
ggplot(steps3, aes(x=interval, y = average)) + geom_line()+facet_grid(day ~ .)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
