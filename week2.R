#loading necessary libraries
library(readr)
library(tidyverse)

# Specifying the URL of the zip file
zip_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Specify the destination to save the downloaded file in the Git Repo
zip_file <- "activity.zip"

# Download the file
download.file(zip_url, zip_file, mode = "wb")

# Unzip the downloaded file
unzip(zip_file)

# listing the files extracted
list.files()

#loading the file
library(readr)
activity <- read_csv("activity.csv")
View(activity)

dim(activity)
head(activity)

#1.calculating total steps taken per day
ts <- activity |> 
        group_by(date) |> 
        summarize(total_steps = sum(steps, na.rm=TRUE))

print(ts, n=61)

# 2.plotting histogram with steps per day
ggplot(data=ts, aes(x=total_steps))+geom_histogram(bins = 61)

# 3. Calculate and report the mean and median of the total number of steps taken per day
summary(ts$total_steps, na.rm=TRUE)

#1. Make a time series
## Calculate the average number of steps per interval
average_steps <- activity %>%
        group_by(interval) %>%
        summarize(avg_steps = mean(steps, na.rm = TRUE))

str(average_steps)

#Create a times series plot
ggplot(average_steps, aes(x = interval, y = avg_steps)) +
        geom_line() +
        labs(title = "Average Number of Steps per 5-Minute Interval",
             x = "5-Minute Interval",
             y = "Average Number of Steps")
       