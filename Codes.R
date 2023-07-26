#*******************************************************************************
### Install required libraries and set global markdown options
library(tidyverse)


#*******************************************************************************
### Read and pre-process data
# file.URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(file.URL, file.dir, method = "auto")


file.name <- "activity.csv"
file.dir <- "./activity.zip"
unzip(file.dir)
activity <- read.csv(file.name, na.strings = "NA")
file.remove(file.name)
rm(file.dir, file.name)

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")


#*******************************************************************************
### What is mean total number of steps taken per day?

# Total number of steps taken per day, mean and median
total.daily.steps <-
        with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean.daily.steps <- mean(total.daily.steps)
median.daily.steps <- median(total.daily.steps)


# histogram of the total number of steps taken each day
hist(
        x = total.daily.steps,
        breaks = 8,
        col = "cornflowerblue",
        main = "Total number of steps taken each day",
        xlab = "Total daily steps",
        ylab = "Count",
        ylim = c(0, 25)
)


#*******************************************************************************
### What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)

daily.steps <- activity %>%
        group_by(interval) %>%
        summarise("Average.Daily.Steps" = mean(steps, na.rm = TRUE))

interval.breaks <- c(0, 392.5, 785, 1177.5, 1570, 1962.5, 2355)
interval.labels <- c("00:00", "04:00", "08:00",
                     "12:00", "16:00", "20:00", "24:00")
with(
        daily.steps,
        plot(
                x = interval,
                y = Average.Daily.Steps,
                type = "l",
                main = "Average daily steps in the 5-min interval",
                xlab = "5-minute interval over 24 hours",
                ylab = "Average steps over 61 days",
                xaxt = "n"
        )
)
axis(1, at = interval.breaks, labels = interval.labels)


# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
max.step.index <- which.max(daily.steps$Average.Daily.Steps)
time_start <- sprintf("%02d:%02d", ((max.step.index * 5) %/% 60),
                      ((max.step.index * 5) %% 60))
time_end <- sprintf("%02d:%02d", ((max.step.index * 5 + 5) %/% 60),
                    ((max.step.index * 5 + 5) %% 60))
max.steps <- daily.steps$Average.Daily.Steps[max.step.index]
cat(paste0(
        "From ",
        time_start,
        " - ",
        time_end,
        "AM with ",
        format(max.steps, digits = 3) ,
        " steps."
))


#*******************************************************************************
### Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing.obs <- length(activity[rowSums(is.na(activity)) > 0, 1])
colSums(is.na(activity))
round(missing.obs / nrow(activity) * 100, 2)


# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

missing.index <- which(rowSums(is.na(activity)) > 0)
length(missing.index)

# checking if steps is missing for all intervals or entire day
activity$interval[missing.index]

# see count of missing values by date
table(activity$date[missing.index])
## 8 days with missing data for full day
## 8 * 288 = 2,304 === total missing values


# create a copy of data frame
activity2 <- activity

# replace missing values with average number of steps in the 5-min interval
# average daily steps for 5-min interval (288 values) repeated 8 times
# for 8 missing days == 2,304
activity2$steps[missing.index] <-
        rep(daily.steps$Average.Daily.Steps, 8)
activity2$steps <- as.integer(activity2$steps)

# checking for NAs after imputation
colSums(is.na(activity2))


# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

total.daily.steps2 <-
        with(activity2, tapply(steps, date, sum))
mean.daily.steps2 <- mean(total.daily.steps2)
median.daily.steps2 <- median(total.daily.steps2)


# histogram of the total number of steps taken each day
hist(
        x = total.daily.steps2,
        breaks = 8,
        col = "cornflowerblue",
        main = "Total number of steps taken each day (imputed data)",
        xlab = "Total daily steps",
        ylab = "Count",
        ylim = c(0, 25)
)

# comparison via panel plot

library(lattice)
df <-
        data.frame(
                "Steps" = c(total.daily.steps, total.daily.steps2),
                "Group" = c(rep("Raw data", 61), rep("Imputed data", 61))
        )
head(df)
histogram(
        ~ Steps | Group,
        data = df,
        type = "count",
        breaks = 11,
        main = "Total number of steps taken each day",
        xlab = "Total daily steps",
        ylab = "Count",
        ylim = c(0, 25)
)


#*******************************************************************************
### Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity2$Day.Class <-
        ifelse((weekdays(activity2$date, TRUE) %in% c("Sat", "Sun")),
               yes = "Weekend", no = "Weekday")


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
average.by.day.of.week <- activity2 %>%
        group_by(Day.Class, interval) %>%
        summarise(Avg = mean(steps))

average.by.day.of.week %>%
        ggplot(aes(x = interval, y = Avg, group = Day.Class)) +
        geom_line(aes(color = Day.Class)) +
        facet_grid(rows = vars(Day.Class)) +
        theme_light() +
        labs(
                title = "Activity patterns between weekdays and weekends",
                x = "5-minute interval over 24 hours",
                y = "Number of steps in 5 minutes, averaged over 61 days",
                group = "Day Type"
        ) +
        scale_x_continuous(breaks = interval.breaks, labels = interval.labels)

#*******************************************************************************
#*******************************************************************************
#*******************************************************************************