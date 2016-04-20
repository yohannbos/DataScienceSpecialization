
#Reproducible Research: Peer Assessment 1
##Loading and preprocessing the data

```{r, echo=TRUE}
d<-getwd()
url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,file.path(d, "activity.zip"), mode="wb")

unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```
##What is mean total number of steps taken per day?

```{r, echo=TRUE}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total number of steps per day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```
##What is the average daily activity pattern?

```{r, echo=TRUE}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```
On average across all the days in the dataset, the 5-minute interval containing the maximum number of steps is:

```{r, echo=TRUE}
averages[which.max(averages$steps),]
```
##Imputing missing values

There are many days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

```{r, echo=TRUE}
missing.values <- is.na(data$steps)
```
# How many missing

```{r, echo=TRUE}
table(missing.values)
```
All of the missing values are filled in with mean value for that 5-minute interval.

```{r, echo=TRUE}
imput.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(imput.value, filled.data$steps, filled.data$interval)
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.
```{r, echo=TRUE}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Total number of steps per day")
mean(total.steps)
median(total.steps)
```
Mean and median values are higher after imputing missing data, because we replaced all missing values of the original data set by the mean value. . The total number of steps taken in such days was set to 0 by default.These 0 values now have been removed from the histogram.

##Are there differences in activity patterns between weekdays and weekends?

Find the day of the week for each measurement in the dataset (using the dataset with the filled-in values):


```{r, echo=TRUE}
wday.or.wend <- function(da) {
    day <- weekdays(da)
    if (day %in% c("lundi", "mardi", "mercredi", "jeudi", "vendredi"))
        return("weekday")
    else if (day %in% c("samedi", "dimanche"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=wday.or.wend)
```
Now, let's make a panel plot containing plots of average number of steps taken on weekdays and weekends.

```{r, echo=TRUE}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```