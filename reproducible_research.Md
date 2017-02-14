---
title: "Peer Graded Assignment: Course Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Valon Tika
###January 24, 2017

This is the first peer-graded assignment for the Reproducible Research course in Data Science Specialization. The analysis goes through and answers the questions that were given from the professor on a set of data from a personal activity monitoring device, that collects at 5 minute intervals throughout the day the activity of an individual. The file will present the code, Note, that this code will set your working directory to your C drive, to keep things straight forward. 



## 1. This step loads the packages

```{r }
##set directory and load packages##
setwd("C:/Users/vtika/Desktop/R Programming/Reproducible Research/Week 2/Assignment/")

library(ggplot2)
library(scales)
library(dplyr)
library(data.table)
library(Hmisc)

```



## 2. Loading the dataset 
```{r load dataset, echo = TRUE }
##load dataset##
activity <- "repdata%2Fdata%2Factivity.zip"

if (!file.exists(activity)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, activity)
}  
if (!file.exists("activity.csv")) { 
  unzip(activity) 
}

activity.data <- read.csv("activity.csv", header = TRUE)
```

## 3. Processed dataset to a suitable dataset for the analysis

```{r Perform ETL Procedure to get data ready for analysis, echo = TRUE}
activity.data$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activity.data$interval), format='%H:%M')
```

## 4. Calculate the mean of steps taken per day

A. prepare data

```{r prepare dataset, echo = TRUE }
stepsbyday <- tapply(activity.data$steps, activity.data$date, sum, na.rm = TRUE)
```

B.Make a historgram of the total numbers of steps taken each day

```{r create historgram, echo = TRUE }
hist(stepsbyday, xlab = 'Total Steps per day', 
     ylab = 'Frequency', 
     col = 'light blue',
     ylim = c(0,35),
     main = "Total Number of steps taken each day")
```
k
C. Calculate the mean and median total number of steps taken per day
```{r calculate mean, echo = TRUE}
meanstepsbyday <- mean(stepsbyday)
```

## Answer: [1] 9354.23

```{r calculate median, echo = TRUE}
medianstepsbyday <- median(stepsbyday)
```

## Answer: [1] 10395

## 5. What is the average daily activity pattern?

A. Times series plot preparation

```{r echo = TRUE}
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity.data$steps), by=list(interval=activity.data$interval), FUN=mean, na.rm=TRUE)
```

B. create time series plot
```{R echo = TRUE}
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanstepsbyday)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

C.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo = TRUE}
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

6. Impute MIssing Values

A. Calculate and report the total number of missing values in the dataset 
```{r echo=TRUE}
numMissingValues <- length(which(is.na(activity.data$steps)))
```


B. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
C. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r echo=TRUE}
activityDataImputed <- activity.data
activityDataImputed$steps <- impute(activity.data$steps, fun=mean)
```

D. Make a histogram of the total number of steps taken each day 
```{r echo = TRUE}
stepsByDayImputed <- tapply(activityDatAImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

E. Calculate and report the mean and median total number of steps taken per day. 

```{r echo =TRUE}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```


7. Are there differences in activity patterns between weekdays and weekends?

A. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r echo = TRUE}
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activity.dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

B. Make a panel plot containing a time series plot

```{r echo = TRUE}
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```


