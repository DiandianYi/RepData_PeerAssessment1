---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
```{r}
getwd()
setwd("C:/Users/Diandian/Documents/self_education/Working-directory/repdata_data_activity")
activity <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?
1.Extract the weekdays of the dates

```{r}
activity$weekdays <- weekdays(as.Date(activity[,2]))
```

2.Calculate the total number of steps taken perday

```{r}
totalstep_perday <- tapply(activity$steps,activity$date,sum)
Tstep_perday <- cbind(names(totalstep_perday),totalstep_perday)
Tstep_perday <- as.data.frame(Tstep_perday)
row.names(Tstep_perday) <- seq_len(nrow(Tstep_perday))
colnames(Tstep_perday) <- c("dates","Tstep")
Tstep_perday$Tstep <- as.numeric(as.character(Tstep_perday$Tstep))
```

3.Make a histogram of the total number of steps taken each day

```{r}
hist(Tstep_perday$Tstep,xlab="Steps per day",main = "Histogram of the total number of steps taken each day", labels = TRUE)
```

4.Report the mean and median of the total number of steps taken per day

```{r}
summary(Tstep_perday$Tstep)
```

## What is the average daily activity pattern?
1.Make a time series plot and the average number of steps taken

```{r}
plot(activity$steps,type = "l",xlab="ID for time intervals",ylab="steps",main = "Time series plot for the 5-minute interval")
abline(h=mean(activity$steps,na.rm = TRUE),col="blue",lwd=3)
legend("topleft", legend=c("Time series plot", "The average number of steps taken"),
       col=c("black", "blue"), lty=1:1, cex=0.8)
```

2.Determine which 5-minute interval contains teh maximum number of steps

```{r}
activity[which.max(activity$steps),]
```

## Imputing missing values
1.Calculate and report the total number of missing values in the dataset

```{r}
sum(is.na(activity$steps))
```

2.Fill in the missing data with 0

```{r}
activity_nona <- activity
activity_nona[is.na(activity_nona)] <- 0 
```

3.Repeat what we did to get the number of steps per day and report the summary figure

```{r}
totalstep_perday_2 <- tapply(activity_nona$steps,activity_nona$date,sum)
Tstep_perday_2 <- cbind(names(totalstep_perday_2),totalstep_perday_2)
Tstep_perday_2 <- as.data.frame(Tstep_perday_2)
row.names(Tstep_perday_2) <- seq_len(nrow(Tstep_perday_2))
colnames(Tstep_perday_2) <- c("dates","Tstep")
Tstep_perday_2$Tstep <- as.numeric(as.character(Tstep_perday_2$Tstep))

hist(Tstep_perday_2$Tstep,xlab="Steps per day",main = "Histogram of the total number of steps taken each day(imputing NA)", labels = TRUE)

summary(Tstep_perday_2$Tstep)
```

4.Results are different. Treat NA as 0 substantially decreased the median and the mean of the daily number of the steps.

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```{r}
activity_nona$weekday_class <- ifelse(activity_nona$weekdays %in% c("Saturday","Sunday"),"weekend","weekday")
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r}
library(ggplot2)
Average <- tapply(activity_nona$steps,activity_nona$weekday_class,mean)
AverageData <- as.data.frame(cbind(names(Average),Average))
colnames(AverageData) <- c("weekday_class","Average")
plot<- ggplot(activity_nona, aes(x = interval , y = steps, color = weekday_class)) +
    geom_line() +
    labs(title = "time series plot of steps by weekdays", x = "Interval", y = "steps") +
    facet_wrap(~weekday_class, ncol = 1, nrow=2)+
    geom_hline(data = AverageData, aes(yintercept = as.numeric(as.character(AverageData$Average))))
print(plot)
```