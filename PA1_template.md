Course Project
==============

Loading and preprocessing the data
----------------------------------

    #1
    setwd("C:/Users/MS/Desktop/Coursera/JH Data Science/Projects/Proyectos/Reproductible Research/W1")

    activity <- read.csv("activity.csv")

    #2
    activity$date <- as.Date(activity$date,"%Y-%m-%d")

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

What is mean total number of steps taken per day?
-------------------------------------------------

    #1
    steps.day <- group_by(activity, date) %>%
            summarize(Steps.per.day=sum(steps))

    #2
    hist(steps.day$Steps.per.day, breaks = 30, col="grey", main= "Frecuency Steps per Day", xlab= "N of Steps")

![](PA1_template_files/figure-markdown_strict/mean-1.png)

    #3
    mean.steps <- mean(steps.day$Steps.per.day, na.rm = T)
    median.steps <- median(steps.day$Steps.per.day, na.rm = T)

The mean and median of the total number of steps taken per day are:

-Mean: **1.076618910^{4}**

-Median: **10765**

What is the average daily activity pattern?
-------------------------------------------

    #1
    avgsteps.interval <- group_by(activity, interval) %>%
            summarize(Avg.steps.interval=mean(steps, na.rm=T))

    plot(Avg.steps.interval~interval, data=avgsteps.interval, type="l", main= "Average Steps (per day) in 5 min Interval", xlab= "5 min Interval", ylab= "Average Steps per Day")

![](PA1_template_files/figure-markdown_strict/meanstepstaken-1.png)

    #2
    max.interval <- avgsteps.interval$interval[which.max(avgsteps.interval$Avg.steps.interval)]

The **835** interval is the 5-minute interval that contains the maximum
number of steps

Imputing missing values
-----------------------

    #1
    missing.val <- sum(!complete.cases(activity))

    #2 & 3

    activity2 <- activity %>%
            left_join(avgsteps.interval, by="interval")

    activity2$steps <- ifelse(is.na(activity2$steps), activity2$Avg.steps.interval, activity2$steps)

    activity2.day <- group_by(activity2, date) %>%
            summarize(Steps.per.day=sum(steps))


    #4
    hist(activity2.day$Steps.per.day, breaks = 30, main= "Frecuency Steps per Day (No NAs)", xlab= "N of Steps")

![](PA1_template_files/figure-markdown_strict/missingval-1.png)

    mean.steps2 <- mean(activity2.day$Steps.per.day, na.rm = T)
    median.steps2 <- median(activity2.day$Steps.per.day, na.rm = T)

There is a total of **2304** missing values in the dataset

The mean and median total number of steps taken per day are:

-Mean: **1.076618910^{4}**

-Median: **1.076618910^{4}**

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    #1
    activity2$weekday <- ifelse(weekdays(activity2$date) %in% 
                                            c("sábado", "domingo"), 
                                            "weekend", "weekday")
    #2
    activity2 <- activity2 %>% 
            group_by(interval, weekday) %>% 
            summarise(mean.steps = mean(steps, na.rm =T))

    library(ggplot2)

    ggplot(data = activity2, mapping = aes(x = interval, y = mean.steps)) + 
            geom_line() + facet_grid(weekday ~ .) + 
            scale_x_continuous("Intervals",breaks = seq(min(activity2$interval), max(activity2$interval), 100)) + 
            scale_y_continuous("Avg N of Steps") + ggtitle("Average Number of Steps Taken by Interval")

![](PA1_template_files/figure-markdown_strict/differences-1.png)

There are diferences between the two. In weekdays there is a clear peak
in activity in the morning (Probably because people have to work later).
On the other hand, on weekends there are various peaks through the day.
