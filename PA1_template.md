Loading and preprocessing the data
----------------------------------

1.  & 2. Code used to load the data (no preprocessing necessary) -

<!-- -->

    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
    activity <- read.csv(unz(temp, "activity.csv"), na.strings = "NA")
    unlink(temp)

What is the mean total number of steps taken per day?
-----------------------------------------------------

1.  Code to calculate the total number of steps taken each day -

<!-- -->

    tot_daily_steps <- with(activity, aggregate(steps, by = list(date), sum))
    colnames(tot_daily_steps) <- c("date", "total_steps")

1.  Code to plot a histogram of the total number of steps taken each
    day -

<!-- -->

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.5

    hist1 <- ggplot(tot_daily_steps, aes(total_steps)) 
    hist1 + geom_histogram() + labs(title = "Step Total per Day", x = "Step Total")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)<!-- -->

1.  Code to calculate the mean & median of the total number of steps
    taken each day -

<!-- -->

    summary <- summary(tot_daily_steps$total_steps)
    paste("Mean total daily steps =", summary[4], "& median total daily steps =", summary[3])

    ## [1] "Mean total daily steps = 10770 & median total daily steps = 10760"

What is the average daily activity pattern?
-------------------------------------------

1.  Code to plot a time series of 5-min interval vs average steps taken
    per interval (averaged over all days) -

<!-- -->

    av_int_steps <- with(activity, aggregate(steps, by = list(interval), mean, na.rm = TRUE))
    colnames(av_int_steps) <- c("interval", "average_steps")
    with(av_int_steps, plot(interval, average_steps, type = "l", main = " Average Steps per Interval"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)<!-- -->

1.  Code to determine which interval contains the maximum average number
    of steps -

<!-- -->

    max_av_int_steps <- which.max(av_int_steps$average_steps)
    print(paste("The interval which on average contains the maximum number of steps is", av_int_steps$interval[max_av_int_steps]))

    ## [1] "The interval which on average contains the maximum number of steps is 835"

Imputing missing values
-----------------------

1.  Code to calculate the number of rows where there are missing values
    (NAs) -

<!-- -->

    missing <- sum(is.na(activity))
    print(paste(missing, "rows contain missing data"))

    ## [1] "2304 rows contain missing data"

1.  Strategy for imputing missing data: It was considered that when
    imputing missing values for the number of steps per interval it
    would be most appropriate to use the mean for the missing interval
    rather than the daily mean as there is likely to be a greater
    difference in movement throughout a 24h period rather than between
    days.

2.  Code to create a dataset called activity2 where the missing values
    for the number of steps per interval are filled in with the mean
    calculated for that 5 min interval -

<!-- -->

    ## calculate df correlating interval with averages ##

    av_int_steps <- with(activity, aggregate(steps, by = list(interval), mean, na.rm = TRUE))
    colnames(av_int_steps) <- c("interval", "average_steps")

    ## add column to activity dataset for measured + imputed data ##

    activity$steps_noNA <- activity$steps

    ## merge activity and av_int_steps datasets by interval to create column of interval averages in the activity dataset ##

    activity <- merge(av_int_steps, activity, by.y = "interval", all = TRUE)

    ## ID which rows in activity dataset contain NAs for the number of steps taken ##

    NA_index <- is.na(activity$steps_noNA)

    ## for each of these rows replace NAs in steps_noNA column with average_steps from same row  ##

    activity$steps_noNA[NA_index] <- activity$average_steps[NA_index]

    ##create new dataframe containing only columns of original dataframe ##

    activity2 <- activity[,c("interval", "steps_noNA", "date")]
    colnames(activity2) <- c("interval", "steps", "date")

1.  Code to plot a histogram of the total number of steps taken each day
    using the activity2 dataset (missing values are imputed) and code to
    report the mean and median number of steps per interval for the same
    dataset -

<!-- -->

    ## calculate total daily steps from activity2 dataset ##

    tot_daily_steps2 <- with(activity2, aggregate(steps, by = list(date), sum))
    colnames(tot_daily_steps2) <- c("date", "total_steps")

    ## plot histogram of total daily steps from activity2 dataset ##
    library(ggplot2)
    hist1 <- ggplot(tot_daily_steps2, aes(total_steps)) 
    hist1 + geom_histogram() + labs(title = "Step Total per Day (including imputed data)", x = "Step Total")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)<!-- -->

    ## calculate the mean & median of the total daily steps from activity 2 dataset and reprint the equivalents for the activity dataset for easy comparison ##

    summary2 <- summary(tot_daily_steps2$total_steps)
    paste("Mean total daily steps (including imputed data) =", summary2[4], "& median total daily steps (including imputed data) =", summary2[3])

    ## [1] "Mean total daily steps (including imputed data) = 10770 & median total daily steps (including imputed data) = 10770"

    paste("Mean total daily steps (including missing data) =", summary[4], "& median total daily steps (including missing data) =", summary[3])

    ## [1] "Mean total daily steps (including missing data) = 10770 & median total daily steps (including missing data) = 10760"

Imputing values for missing data has increased the median value for
total daily steps from 10760 to 10770, but the mean remains the same at
10770. As the difference is small the impact of imputing missing data is
likely to be small.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

The dataset used to answer this question has replaced missing values for
the number of steps taken per interval with the mean number of steps per
interval. N.B. If the mean daily average had been substituted instead of
the mean interval average, differences between weekdays and weekends
would probably be more exaggerated.

1.  Code to create a factor variable indicating if the date listed is a
    weekday or weekend -

<!-- -->

    activity2$day <- weekdays(as.POSIXlt(activity2$date))
    activity2$day_type <- "weekday"
    saturday <- activity2$day == "Saturday"
    sunday <- activity2$day == "Sunday"
    for(i in saturday){
            activity2$day_type[saturday] <- "weekend"
    }
    for(i in sunday){
            activity2$day_type[sunday] <- "weekend"
    }
    activity2$day_type <- as.factor(activity2$day_type)

1.  Code to produce a panel plot of the average number of steps taken
    per interval averaged across all week days or all weekend days using
    activity 2 dataset which includes imputed values in place of missing
    values.

<!-- -->

    ## calculate df correlating interval with average number of steps separated by weekday vs weekend ##

    df <- with(activity2, aggregate(steps, by = list(day_type, interval), mean, na.rm = TRUE))
    colnames(df) <- c("day_type", "interval", "average_steps")

    ##create plot ##

    library(lattice)
    xyplot(average_steps ~ interval | day_type, df, layout = c(1,2), type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)<!-- -->

Superficial comparison of the 2 plots suggests that on average the
number steps taken throughout the day are fewer on weekdays during what
is most likely to be working hours. Also, on averge more steps are taken
starting earlier in the morning, which again most likely correlates with
people getting up earlier and going to work.
