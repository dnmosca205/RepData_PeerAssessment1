# Reproducible Research: Peer Assessment 1

## Loading and preprocessing data

* Data is downloaded if not present .
* Zip file downloaded is uncompressed.
* Data is preprocessed .
* Data is inxpected .


```r
getData <- function() {

    install.packages("downloader",repos="http://cran.rstudio.com/")
    require(downloader)
    url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  
    
    zip_source_name = "activity.zip"
    
    print("check file")
    
    if(!file.exists(zip_source_name)) {
    
            print("download file")
            download(url,destfile=zip_source_name, mode="wb")          
    }
    
    if (!"act" %in% ls()) { 
                  
            print("unzip file")
            act <- unzip(zip_source_name)
    }   
 
    actData <- read.csv(act, header=T, colClasses = c("integer", "Date", "factor"))
    actData$interval <- factor(actData$interval)
    actData$date <- as.Date(actData$date, format="%Y-%m-%d")
    actData
}
actData <- getData()
```

```
## Installing package into 'C:/Users/Danilo/Documents/R/win-library/3.1'
## (as 'lib' is unspecified)
```

```
## Warning: package 'downloader' is in use and will not be installed
```

```
## [1] "check file"
## [1] "unzip file"
```

Inspect data.

```r
head(actData)
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

```r
dim(actData)
```

```
## [1] 17568     3
```

```r
summary(actData)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   61  
##  Median :  0.00   Median :2012-10-31   100    :   61  
##  Mean   : 37.38   Mean   :2012-10-31   1000   :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   1005   :   61  
##  Max.   :806.00   Max.   :2012-11-30   1010   :   61  
##  NA's   :2304                          (Other):17202
```

```r
str(actData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","10","100",..: 1 226 2 73 136 195 198 209 212 223 ...
```

## What is mean total number of steps taken per day?

* Calculate the total number of steps taken per day
* Make a histogram of the total number of steps taken each day
* Calculate and report the mean and median of the total number of steps taken per day


```r
 actDataNoNA <- na.omit(actData)
  
 totalStepsPerDay <- aggregate(steps ~ date, actDataNoNA, sum)

 hist(totalStepsPerDay$steps, breaks = 20, main="Histogram of steps ", xlab="Number of steps taken per day", ylab = "Number of times", col="grey")
```

![plot of chunk stepsPerDay](figure/stepsPerDay-1.png) 
Mean of the total number of steps taken per day

```r
 mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day

```r
 median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
library(ggplot2)

meanSteps <- aggregate(actDataNoNA$steps, list(interval = as.numeric(as.character(actDataNoNA$interval))), FUN = "mean")

names(meanSteps)[2] <- "avgNumberOfSteps"

ggplot(meanSteps, aes(interval, avgNumberOfSteps)) + geom_line(color = "grey", size = 0.6) + labs(title = "Average daily activity pattern", x = "5-minute intervals", y = "Average number of steps")
```

![plot of chunk dailyActivity](figure/dailyActivity-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
    meanSteps[meanSteps$avgNumberOfSteps == max(meanSteps$avgNumberOfSteps), ]
```

```
##     interval avgNumberOfSteps
## 104      835         206.1698
```

## Inputing missing values

* The total number of rows with NAs:


```r
    sum(is.na(actData))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. 

Using the mean for that 5-minute interval


```r
  filledData <- actData 
  for (i in 1:nrow(filledData)) {
      if (is.na(filledData$steps[i])) {
          filledData$steps[i] <- meanSteps[which(filledData$interval[i] == meanSteps$interval), ]$avgNumberOfSteps
      }
  }
  
  # checking data correctness
  head(filledData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
  sum(is.na(filledData))  
```

```
## [1] 0
```
* Histogram of the total number of steps taken each day 

 
 ```r
 ggplot(filledData, aes(date, steps)) + geom_bar(stat = "identity",colour = "grey",fill = "grey",width = 0.7) + labs(title = "Histogram of total number of steps whit filled na data", x = "Date", y = "Steps")
 ```
 
 ![plot of chunk stepsPerDayFilledData](figure/stepsPerDayFilledData-1.png) 

* Mean of the total number of steps taken per day

```r
   totalStepsPerDayFilled <- aggregate(steps ~ date, filledData, sum)
   mean(totalStepsPerDayFilled$steps)
```

```
## [1] 10766.19
```

* Median of the total number of steps taken per day

```r
     median(totalStepsPerDayFilled$steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?


```r
Sys.setlocale("LC_TIME", "English") 
```

```
## [1] "English_United States.1252"
```

```r
filledData$weekdays <- weekdays(as.Date(filledData$date))
filledData$weekdays <- ifelse(filledData$weekdays %in% c("Saturday", "Sunday"),"weekend", "weekday")

meanStepsWk <- aggregate(filledData$steps, 
                      list(interval = as.numeric(as.character(filledData$interval)), 
                           weekdays = filledData$weekdays),
                      FUN = "mean")
                      
names(meanStepsWk)[3] <- "avgNumberOfStepsWk"    

library(lattice)

xyplot(avgNumberOfStepsWk ~ interval | weekdays,data= meanStepsWk,
       layout = c(1, 2), type = "l", col="grey", 
       xlab = "Interval", ylab = "Average number of steps")  
```

![plot of chunk avgNumberOfStepsWk](figure/avgNumberOfStepsWk-1.png) 

    


