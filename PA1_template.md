# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

    echo = TRUE

    activity <- read.csv("activity.csv")


## What is mean total number of steps taken per day?

    stepsperday <- aggregate(steps ~ date, data = activity, FUN = sum)
    mean(stepsperday$steps)
    
#The Mean number of step equals 10766.19

    median(stepsperday$steps)
##The median number of steps equals 10765

## What is the average daily activity pattern?

    steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
    plot(steps.interval, type = "l")

## Inputting missing values

    activity <- merge(activity, stepsbyint, by = "interval", suffixes = c("", ".y"))

    NAVals <- is.na(activity$steps)
    activity$steps[NAVals] <-activity$steps.y[NAVals]

    activity <- activity[, c(1:3)]

    steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
    barplot(steps.date$steps, names.arg = steps.date$date, xlab = "data", ylab = "steps")
    
    ### a. 
    mean(steps.date$steps)
##The resulting value is 10766.19.

### b. 
    median(steps.date$steps)
##The resulting value is 10765.

###Considering the difference between the mean (a) and the median (b), as expected the impact of NA values appears, as expected, minimal, based on the approximate total number of daily steps.

## Max steps on average per five-minute interval across the dataset

    steps.interval$interval[which.max(steps.interval$steps)]

## There are 835 steps per five-minute interval on average.  


## Are there differences in activity patterns between weekdays and weekends?

    newData$weekdays <- factor(format(newData$date, "%A"))
    levels(newData$weekdays)
    levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", 
      "Thursday", "Friday"),
    weekend = c("Saturday", "Sunday"))

    levels(newData$weekdays)

    avgSteps <- aggregate(newData$steps, 
            list(interval = as.numeric(as.character(newData$interval)), 
                  weekdays = newData$weekdays),
                      FUN = "mean")
    
    names(avgSteps)[3] <- "meanofSteps"
    library(lattice)
    xyplot(avgSteps$meanofSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
    table(newData$weekdays)

##There is a 2.428 ratio of weekday/weekend steps.

##weekday weekend 
## 4896 v. 2016 
