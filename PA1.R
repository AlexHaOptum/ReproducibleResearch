require(ggplot2)
library(magrittr)
library(dplyr)

getwd()
setwd("~/Coursera/ReproducibleResearch")

data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)

## What is mean total number of steps taken per day?
avgTotalSteps <- aggregate(df$steps, by = list(df$date), FUN = sum)
colnames(avgTotalSteps) <- c("Date","Total")
hist(avgTotalSteps$Total, xlab = "Average Total Steps", main = "Histogram")
mean(avgTotalSteps$Total)
median(avgTotalSteps$Total)

## What is the average daily activity pattern?
avgStepsPerInterval <- aggregate(df$steps, by = list(df$interval), FUN = sum)
colnames(avgStepsPerInterval) <- c("Interval", "Average")
plot(avgStepsPerInterval$Interval, avgStepsPerInterval$Average,
     axes = F, type = "l", col = "orange", xlab = "Time", 
     ylab = "Average Number of Steps", main = "Average Daily Activity")
axis(1, at = c(0, 600, 1200, 1800, 2400), label = c("12:00 AM","6:00 AM","12:00 PM","6:00 PM","12:00 AM"))
axis(2)

avgStepsPerInterval[which.max(avgStepsPerInterval$Average),]

## Imputing missing values

dfNA <- data.frame(data)
sum(is.na(dfNA$steps))

## Strategy for filling in all missing values in dataset

avgSteps <- aggregate(df$steps, list(interval = as.numeric(as.character(df$interval))), FUN = "mean")
imputed <- data 
for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    imputed$steps[i] <- avgSteps[which(imputed$interval[i] == avgSteps$interval), ]$x
  }
}
imputed$date <- as.Date(imputed$date)
gg <- ggplot(imputed, aes(date, steps)) 
gg <- gg + geom_bar (stat = "identity",color = "orange", fill = "orange", size = 0.5, position = position_dodge(width = 0.25)) 
gg <- gg + labs(title = "Histogram",x = "Date",y = "Total number of steps")
print(gg)
filledTotalSteps <- aggregate(imputed$steps, list (Date = imputed$date), FUN = "sum")$x
mean(filledTotalSteps)
median(filledTotalSteps)

## Median is increased

## Are there differences in activity patterns between weekdays and weekends?

dfDay <- imputed

dfDay <- dfDay %>%
  mutate(typeOfDay = ifelse(weekdays(dfDay$date) == "Saturday" | 
                              weekdays(dfDay$date) == "Sunday", "Weekend", "Weekday"))
head(dfDay)

dayVsEnd <- dfDay%>%
            group_by(interval, typeOfDay) %>%
            summarise(avg_steps2 = mean(steps, na.rm = TRUE))
  
dayVsEndgg <- ggplot(dayVsEnd, aes(x =interval , y=avg_steps2, color=typeOfDay)) +
  geom_line() +
  labs(title = "Avg Daily Steps", x = "Interval", y = "No. of Steps")
print(dayVsEndgg)