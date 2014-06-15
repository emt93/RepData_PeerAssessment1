Reproducible Research: Peer Assessment #1
=======================================================

### Load the activity.csv file into your working directory

### Load packages which are in the library
library(knitr)

### Loading and initial processing the data
Loading the data into R

```r
activity <- read.csv("activity.csv")
```

Transforming the data using the strptime function

```r
activity$date <- as.Date(activity$date,"%Y-%m-%d")
```

### PART #1: What is mean total number of steps taken per day?
Here's a histogram of the total number of steps that were taken each day
*Remove all of the NA values
*Aggregate the data to find the steps per day (sumStepDay)
*Plot the histogram of the aggregate steps per day
        

```r
removeNA <- activity[which(activity$steps!="NA"), ]
aggStepDay <- aggregate(removeNA$steps,removeNA[2],sum)
par(mfrow = c(1,1))
barplot(aggStepDay$x,names.arg. = aggStepDay$date, col="blue",main = "Aggregate Steps/Day",xlab = "Date",ylab = "# of Steps")
```

```
## Warning: "names.arg." is not a graphical parameter
## Warning: "names.arg." is not a graphical parameter
## Warning: "names.arg." is not a graphical parameter
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


```r
dev.off()
```

```
## null device 
##           1
```
Mean and median for the total steps/day

```r
mean(aggStepDay$x)
```

```
## [1] 10766
```

```r
median(aggStepDay$x)
```

```
## [1] 10765
```


### PART #2: What is the average daily activity pattern?
Here's a time series plot (i.e. type = "l") of the average number of steps/interval

```r
avgStepInt <- aggregate(activity$steps,activity[3],mean,na.rm=TRUE)
plot(x~interval,data = avgStepInt, type = "l",xlab = "Interval",ylab = "Average Steps",main = "Average Daily Activity Pattern",col = "red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
### Question: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgStepInt[c(which(avgStepInt$x==max(avgStepInt$x)),which(avgStepInt$x==max(avgStepInt$x))+1),1]
```

```
## [1] 835 840
```
### PART #3: Imputing missing values
Calculate the total number of missing values (i.e. NAs) in the dataset 

```r
nrow(activity[(!complete.cases(activity)),])
```

```
## [1] 2304
```
Define a strategy to replace all of the NA values by replacing them with the mean value previously found (avgStepInt)

```r
avgStepInt <- aggregate(activity$steps,activity[3],mean,na.rm=TRUE)
new <- merge(x=activity,y=avgStepInt,by="interval",all.x=TRUE)
new$steps <- ifelse(is.na(new$steps),round(new$x,digits=2),new$steps)
fin <- new[,-c(4)]
```
Here's the new histogram with all of the NA values substituted by avgStepInt

```r
his <- aggregate(fin$steps,fin[3],sum)
par(mfrow = c(1, 1))
barplot(his$x,names.arg = his$date,col = "green",main = "Total Steps/Day",xlab = "Date",ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


```r
dev.off()
```

```
## null device 
##           1
```

Mean and median for the new total steps/day

```r
mean(his$x)
```

```
## [1] 10766
```

```r
median(his$x)
```

```
## [1] 10766
```

### PART #4: Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels by using the dataset that was treated for missing values


```r
factor <- fin
factor$flag <- as.factor(ifelse(weekdays(fin$date) %in% c("Saturday","Sunday"),"Weekend","Weekdays"))
head(factor)
```

```
##   interval steps       date     flag
## 1        0  1.72 2012-10-01 Weekdays
## 2        0  0.00 2012-11-23 Weekdays
## 3        0  0.00 2012-10-28  Weekend
## 4        0  0.00 2012-11-06 Weekdays
## 5        0  0.00 2012-11-24  Weekend
## 6        0  0.00 2012-11-15 Weekdays
```

Create a panel plot containing a time serie of type = "l"

```r
finale <- aggregate(factor$steps,factor[c(1,4)],mean)
```

Create a few panel plats 

```r
library(lattice)
xyplot(x ~ interval | flag,data = finale,layout=c(1,2),type = "l",xlab = "Interval",ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

