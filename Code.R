library(dplyr)
#Taking and readindg dataset.
unzip("repdata_data_activity.zip")
data <- read.csv("activity.csv")

#Converting data from "character" to "date".
data$date <- as.Date(data$date)

#PART 1

#Histogram from total steps taken by date.
StepsPerDay <- aggregate(data$steps ~ data$date, data = data, 
                         FUN = sum, na.rm = FALSE)
names(StepsPerDay) <- c("date", "steps")
TotalStepsByDay <- hist(StepsPerDay$steps, col= "green", xlab = "Steps  per day")

#Calculating mean.
mean(StepsPerDay$steps)
#Calculating median.
median(StepsPerDay$steps)

#PART 2
#Calculating the daily mean (average) off steps taken by interval.
MeanPerInterval <- aggregate(data$steps ~ data$interval, data = data,
                       FUN = mean, na.rm = FALSE)
names(MeanPerInterval) <- c("interval", "mean of steps")
plot(`mean of steps`~interval, data = MeanPerInterval,
     type = "l", main = "Average of steps taken by interval", 
     xlab = "Inteval", ylab = "Average of steps")
MaximunStepsByInterval <- MeanPerInterval[which.max(MeanPerInterval$`mean of steps`), ]$interval

##PART 3
#Calculating missing values. 
TotalNAS <- sum(is.na(data$steps))
#Changing NAs by the daily mean. 
NAdata <- data
as.Date(NAdata$date)
#$steps[is.na(NAdata$steps)] <- mean(StepsPerDay$steps)
#Changing NAs by the interval mean. 
NAdata$steps[is.na(NAdata$steps)] <- mean(MeanPerInterval$`mean of steps`)

#Histogram with NA
NAStepsPerDay <- aggregate(NAdata$steps ~ NAdata$date, data = NAdata, 
                         FUN = sum)
names(NAStepsPerDay) <- c("date", "steps")
TotalNAStepsByDay <- hist(NAStepsPerDay$steps, 
                          col = "green", xlab = "Steps per day", 
                          main = "Total steps per day (with missing values imputted")
mean(NAStepsPerDay$steps)
median(NAStepsPerDay$steps)

#PART 4
#Introducing days bi its name. 
NAdata$day <- weekdays(NAdata$date)
#Differencing between weekdays and weekends (create a new variable)
NAdata$daytipe <- ifelse(NAdata$day=="sábado"| NAdata$day=="domingo", "weekend", "weekday")
#Calling ggplot
library(ggplot2)
DayTipeDifferences <- ggplot(NAdata, aes(steps, interval))
DayTipeDifferences + geom_line() + facet_grid(daytipe~.) +
      xlab("Steps") + ylab("Interval") + 
      ggtitle("Average of steps by interval: weekdays and weekends differences")
