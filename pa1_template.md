--- 
title: "Project1" 
output: 
  html_document: 
    keep_md: true 
---
Task 1

```r
stepsdata<-read.csv("activity.csv")
head(stepsdata)
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
Task 2

```r
library(dplyr)
library(magrittr)

daily<-stepsdata %>% 
    select(steps,date) %>% 
    group_by(date) %>%
    summarize(dsteps=sum(steps)) %>% 
    na.omit()
daily
```

```
## # A tibble: 53 × 2
##    date       dsteps
##    <chr>       <int>
##  1 2012-10-02    126
##  2 2012-10-03  11352
##  3 2012-10-04  12116
##  4 2012-10-05  13294
##  5 2012-10-06  15420
##  6 2012-10-07  11015
##  7 2012-10-09  12811
##  8 2012-10-10   9900
##  9 2012-10-11  10304
## 10 2012-10-12  17382
## # … with 43 more rows
```

```r
hist(daily$dsteps,xlab="Daily Steps",main="Histogram of DSteps",breaks=25)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Task 3

```r
daily
```

```
## # A tibble: 53 × 2
##    date       dsteps
##    <chr>       <int>
##  1 2012-10-02    126
##  2 2012-10-03  11352
##  3 2012-10-04  12116
##  4 2012-10-05  13294
##  5 2012-10-06  15420
##  6 2012-10-07  11015
##  7 2012-10-09  12811
##  8 2012-10-10   9900
##  9 2012-10-11  10304
## 10 2012-10-12  17382
## # … with 43 more rows
```

```r
mean(daily$dsteps)
```

```
## [1] 10766.19
```

```r
median(daily$dsteps)
```

```
## [1] 10765
```


Task 4

```r
intervals<-stepsdata %>% 
    na.omit() %>% 
    select(steps,interval) %>% 
    group_by(interval) %>% 
    summarize(dsteps=mean(steps))
intervals
```

```
## # A tibble: 288 × 2
##    interval dsteps
##       <int>  <dbl>
##  1        0 1.72  
##  2        5 0.340 
##  3       10 0.132 
##  4       15 0.151 
##  5       20 0.0755
##  6       25 2.09  
##  7       30 0.528 
##  8       35 0.868 
##  9       40 0     
## 10       45 1.47  
## # … with 278 more rows
```

```r
library(ggplot2)
ggplot(intervals,aes(x=interval,y=dsteps))+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Task 5

```r
intervals[which(intervals$dsteps==max(intervals$dsteps)),]
```

```
## # A tibble: 1 × 2
##   interval dsteps
##      <int>  <dbl>
## 1      835   206.
```

Task 6

```r
missing<-sum(is.na(stepsdata))
missing
```

```
## [1] 2304
```

```r
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepsdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)
```

```
## # A tibble: 6 × 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <chr>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

Task 7

```r
full <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(full)[1] ="date"
names(full)[2] ="totalsteps"
head(full,15)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## 11 2012-10-11   10304.00
## 12 2012-10-12   17382.00
## 13 2012-10-13   12426.00
## 14 2012-10-14   15098.00
## 15 2012-10-15   10139.00
```

```r
hist(full$totalsteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


Task 8

```r
oldmean <- mean(daily$dsteps, na.rm = TRUE)
newmean <- mean(full$totalsteps)

oldmedian <- median(daily$dsteps, na.rm = TRUE)
newmedian <- median(full$totalsteps)
```


Task 9

```r
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
    facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


