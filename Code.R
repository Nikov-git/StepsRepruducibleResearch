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
#Calculating the daily mean (average) off steps taken
MeanPerInterval <- aggregate(data$steps ~ data$interval, data = data,
                       FUN = mean, na.rm = FALSE)
names(MeanPerInterval) <- c("interval", "mean of steps")
plot(`mean of steps`~interval, data = MeanPerInterval,
     type = "l", main = "Average of steps taken by interval", 
     xlab = "Inteval", ylab = "Average of steps")
MaximunStepsByInterval <- MeanPerInterval[which.max(MeanPerInterval$`mean of steps`), ]$interval

                                          