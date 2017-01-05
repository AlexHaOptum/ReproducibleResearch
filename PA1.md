---
title: "Coursera - Reproducible Research Project 1"
author: "Alex Ha"
date: "December 29, 2016"
output: html_document
---

# Installing packages
First, installing the packages/libraries necessary to carry out the program. Also getting/setting the working directory in case it is necessary
```
library(ggplot2)
library(magrittr)
library(dplyr)
library(knitr)
getwd()
setwd("~/Coursera/ReproducibleResearch")
```
## Reading CSV file & omitting null rows
```
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
```
## Question one asks: What is mean total number of steps taken per day?

Calculating the total number of steps taken per day
```
avgTotalSteps <- aggregate(df$steps, by = list(df$date), FUN = sum)
colnames(avgTotalSteps) <- c("Date","Total")
```
Make a histogram of the total number of steps taken each day
``` {r echo = FALSE}
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgTotalSteps <- aggregate(df$steps, by = list(df$date), FUN = sum)
colnames(avgTotalSteps) <- c("Date","Total")
hist(avgTotalSteps$Total, xlab = "Average Total Steps", main = "Histogram")
```


Calculate and report the mean and median of the total number of steps taken per day

Mean
``` {r echo = FALSE}
library(ggplot2)
library(magrittr)
library(knitr)
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgTotalSteps <- aggregate(df$steps, by = list(df$date), FUN = sum)
colnames(avgTotalSteps) <- c("Date","Total")
mean(avgTotalSteps$Total)
```
Median
``` {r echo = FALSE}
library(ggplot2)
library(magrittr)
library(knitr)
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgTotalSteps <- aggregate(df$steps, by = list(df$date), FUN = sum)
colnames(avgTotalSteps) <- c("Date","Total")
median(avgTotalSteps$Total)
```
## What is the average daily activity pattern?
```
avgStepsPerInterval <- aggregate(df$steps, by = list(df$interval), FUN = sum)
colnames(avgStepsPerInterval) <- c("Interval", "Average")
plot(avgStepsPerInterval$Interval, avgStepsPerInterval$Average,
     axes = F, type = "l", col = "orange", xlab = "Time", 
     ylab = "Average Number of Steps", main = "Average Daily Activity")
axis(1, at = c(0, 600, 1200, 1800, 2400), label = c("12:00 AM","6:00 AM","12:00 PM","6:00 PM","12:00 AM"))
axis(2)

avgStepsPerInterval[which.max(avgStepsPerInterval$Average),]
```
## Strategy for filling in all missing values in dataset
```{r}
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgSteps <- aggregate(df$steps, list(interval = as.numeric(as.character(df$interval))), FUN = "mean")
imputed <- data 
for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    imputed$steps[i] <- avgSteps[which(imputed$interval[i] == avgSteps$interval), ]$x
  }
}
imputed$date <- as.Date(imputed$date)
```

Mean
```{r echo = FALSE}
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
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
```

Mean
```{r echo = FALSE}
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgSteps <- aggregate(df$steps, list(interval = as.numeric(as.character(df$interval))), FUN = "mean")
imputed <- data 
for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    imputed$steps[i] <- avgSteps[which(imputed$interval[i] == avgSteps$interval), ]$x
  }
}
imputed$date <- as.Date(imputed$date)
filledTotalSteps <- aggregate(imputed$steps, list (Date = imputed$date), FUN = "sum")$x
mean(filledTotalSteps)
```

Median
```{r echo = FALSE}
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgSteps <- aggregate(df$steps, list(interval = as.numeric(as.character(df$interval))), FUN = "mean")
imputed <- data 
for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    imputed$steps[i] <- avgSteps[which(imputed$interval[i] == avgSteps$interval), ]$x
  }
}
imputed$date <- as.Date(imputed$date)
filledTotalSteps <- aggregate(imputed$steps, list (Date = imputed$date), FUN = "sum")$x
median(filledTotalSteps)
```
## Are there differences in activity patterns between weekdays and weekends?
```{r echo = FALSE}
library(ggplot2)
library(magrittr)
library(dplyr)
library(knitr)
data <- read.csv("activity.csv")
df <- data.frame(data)
df <- na.omit(df)
avgSteps <- aggregate(df$steps, list(interval = as.numeric(as.character(df$interval))), FUN = "mean")
imputed <- data 
for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    imputed$steps[i] <- avgSteps[which(imputed$interval[i] == avgSteps$interval), ]$x
  }
}
imputed$date <- as.Date(imputed$date)

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
```