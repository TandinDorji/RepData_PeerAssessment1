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

total.daily.steps <-
        with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean.daily.steps <- mean(total.steps)
median.daily.steps <- median(total.steps)

hist(
        x = total.daily.steps,
        breaks = 8,
        col = "cornflowerblue",
        main = "Total number of steps taken each day",
        xlab = "Total daily steps",
        ylab = "Count"
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
with(daily.steps, plot(x = interval, y = Average.Daily.Steps, type="l",
                       main = "Average daily steps in the 5-min interval",
                       xlab = "5-minute interval over 24 hours",
                       ylab = "Average steps over 61 days",
                       xaxt = "n"))
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
missing.obs <- length(activity[rowSums(is.na(activity))>0, 1])
colSums(is.na(activity))
round(missing.obs/nrow(activity) * 100, 2)


# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

missing.index <- which(rowSums(is.na(activity))>0)
length(missing.index)

activity$interval[missing.index]
table(activity$date[missing.index])

activity2 <- activity
activity2$steps[missing.index] <- rep(daily.steps$Average.Daily.Steps, 8)
activity2$steps <- as.integer(activity2$steps)

colSums(is.na(activity2))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

mean.steps.2 <- mean(activity2$steps)
median.steps.2 <- median(activity2$steps)

steps.hist.2 <- ggplot(data = activity2) + 
        geom_histogram(mapping = aes(x=steps, fill=..count..), binwidth = 50) + 
        labs(title = "Distribution of daily number of steps (imputed data)", 
             x = "Number of steps taken in 5 minute intervals each day",
             y = "Count") + 
        theme_minimal() +
        theme(
                legend.position = "none"
        ) + 
        scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000),
                           limits = c(0, 15000))
steps.hist.2

#*******************************************************************************
### Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity2$Day.Class <- ifelse((weekdays(activity2$date, TRUE) %in% c("Sat", "Sun")), yes = "Weekend", "Weekday")


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
                title = "Average number of steps in 5-min, by type of day",
                x = "5-minute interval over 24 hours",
                y = "Average steps over 61 days",
                group = "Day Type"
        ) + 
        scale_x_continuous(breaks = interval.breaks, labels = interval.labels)

#*******************************************************************************
#*******************************************************************************
#*******************************************************************************