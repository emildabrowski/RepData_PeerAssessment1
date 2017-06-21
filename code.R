# Loading and preprocessing the data
unzip ("activity.zip")
activity <- read.csv("activity.csv" , sep = "," , header = TRUE)

#load libraries
library(chron)
library(ggplot2)

# What is mean total number of steps taken per day?
#prepare data + mean + median
steps <- tapply(activity$steps , activity$date, FUN = sum , na.rm =TRUE)
meanSteps <- mean(steps, na.rm = TRUE)
medianSteps <- median(steps, na.rm = TRUE)
paste("Mean of steps: " , meanSteps)
paste("Median of steps: " , medianSteps)

#plot
qplot(steps, binwidth=1000, xlab="Mean of total number of steps taken per day")

# What is the average daily activity pattern?
#prepare data for plot
average <- aggregate(x=list(steps = activity$steps) , by = list(interval = activity$interval) , FUN=mean, na.rm=TRUE)
maxAverage <- average[which.max(average$steps),]
paste("This interval contains max average: " , maxAverage$interval)
paste("Average for this interval: " , maxAverage$steps)
ggplot(data=average, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps")

#Imputing missing values

#find missing valurs
missing <- is.na(activity$steps)
paste("Count of missing values: ", table(missing)["TRUE"])

#to fill missing values mean of interval can be used
#search and fill function
meanValue <- function(steps, interval) {
  fill <- NA
  if (!is.na(steps))
    fill <- c(steps)
  else
    fill <- (average[average$interval==interval, "steps"])
  return(fill)
}

#search and fill command
activity$steps <- mapply(meanValue, activity$steps, activity$interval)

#replot
newSteps <- tapply(activity$steps , activity$date, FUN = sum)
newMeanSteps <- mean(newSteps)
newMedianSteps <- median(newSteps)
paste("New mean of steps: " , newMeanSteps)
paste("New median of steps: " , newMedianSteps)
qplot(newSteps, binwidth=1000, xlab="Adjusted mean of total number of steps taken per day")


#Are there differences in activity patterns between weekdays and weekends?
#add boolean to mark weekend
activity$day <- is.weekend(as.Date(activity$date))
labels <- c("TRUE" = "weekend", "FALSE" = "week")
newAverage <- aggregate(steps ~ interval + day, data=activity, mean)
ggplot(newAverage, aes(interval, steps)) + geom_line() + facet_grid(day ~ . , labeller = as_labeller(labels)) + xlab("5-minute interval") + ylab("Steps")
