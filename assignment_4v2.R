# This R code is to outline what is needed to complete Assignment #4 for ANA-515.
# R code below is predominantly from the Uber Data Analysis Project on the Data Flair website.
# https://data-flair.training/blogs/r-data-science-project-uber-data-analysis/
# Code in some instances has been modified to handle errors that cropped up when using 
# code from the Data Flair project. In addition, some modifications have been made to 
# the code to adapt to the Assignment 4 framework or to experiment with features. 
# Robert Brown, ANA-515, Spring 2021.

# Install and import the necessary packages. These included ggplot2, ggthemes, lubridate, dplyer,
# tidyr, DT, scales, and readr. 
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(readr)

# This code gets and establishes the working drive to be a specific folder on my personal computer. 
getwd()
setwd("C:\\Users\\coach\\OneDrive\\Documents\\Data Analytics\\ANA-515 Data Storage, Retrieval and Preparation\\Module 4")
getwd()

# Business Goal of the Project:**
# This project performs some basic analysis and data visualizations of Uber data from April 2014 to September 2014 in New York City. Through this analysis, the business goal is to gain insights into the travel habits of these New York City Uber riders. Uber data sets are stored in a series of zipped csv files on a Google page, linked off of the Data Flair website. 

# As a part of the project, the following packages must be installed: ggplot2, ggthemes, lubridate, dplyer, tidyr, DT, scales, and readr. 
# This creates a vector colors for later use in plotting. 
colors=c("#CC1011","#665555","#05a399","#cfcaca","#f5e840","#0683c9","#e075b0")

# The Uber dataset was downloaded from the Data Flair website. Specifically, 
# they had a Google drive file located at https://drive.google.com/file/d/1emopjfEkTt59jJoBH9L9bSdmlDC4AR87/view
# with compressed files, one per month. I downloaded those to my personal computer and unzipped those to reveal 
# csv files. 
# R Code below uses the read_csv() command to import the data from each month's csv file
# into an appropriately named variable. Afterwards, an rbind() command was used to create
# a single data_2014 dataframe by combining together all each of the monthly dataframes. 
apr_data <- read_csv("uber_raw_data_apr14.csv/uber-raw-data-apr14.csv")
may_data <- read_csv("uber-raw-data-may14.csv/uber-raw-data-may14.csv")
jun_data <- read_csv("uber-raw-data-jun14.csv/uber-raw-data-jun14.csv")
jul_data <- read_csv("uber-raw-data-jul14.csv/uber-raw-data-jul14.csv")
aug_data <- read_csv("uber-raw-data-aug14.csv/uber-raw-data-aug14.csv")
sep_data <- read_csv("uber-raw-data-sep14.csv/uber-raw-data-sep14.csv")
data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

# Command given to view the dataframe called data_2014 and to view the column names. 
View(data_2014)
colnames(data_2014)

# This block of code formats the Date/Time column into Month/Day/Year Hour/Minute/Second
# and then creates objects for day, month, year, day of week, hour, minute, and second. 
# I'm noting here that this code differs from the Data Flair website slightly as they listed 
# initial column as Date.Time when it appeared in dataframe as Date/Time. When I typed in data_2014 with 
# the $ sign to get a specific column, I selected the Data/Time option and it appeared with the 
# `` marks. With that said, it took a little while to figure that out after several errors in running the R
# code prior to that discovery. 
# Code below uses the POSIXct() command. I used the colnames() command a few times to check my work along the way. 
data_2014$`Date/Time` <- as.POSIXct(data_2014$`Date/Time`, format = "%m/%d/%Y %H:%M:%S")
colnames(data_2014)
data_2014$Time <- format(as.POSIXct(data_2014$`Date/Time`, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
colnames(data_2014)
data_2014$`Date/Time` <- ymd_hms(data_2014$`Date/Time`)
data_2014$day <- factor(day(data_2014$`Date/Time`))
data_2014$month <- factor(month(data_2014$`Date/Time`, label = TRUE))
data_2014$year <- factor(year(data_2014$`Date/Time`))
data_2014$dayofweek <- factor(wday(data_2014$`Date/Time`, label = TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))
colnames(data_2014)
# Assignment #4 asks to describe the data set in the R Markdown document using the # rows, #columns, etc. and 
# to give some descriptive statistics. I'll save that code for the R block in the R Markdown document. 
# Given the data in this dataframe, there is very little summary statistics that can be given. What will be
# given in this section is the series of commands to graph the data in a variety of ways, as initially
# demonstrated by Data Flair to practice ggplot2. 

#Plotting the trips by the hours in a day. This creates an hour_data object
# that groups the data by the hour object created above. The summarize function
# from the dplyr library is used to create a total. A data table is created and viewed
# from the hour_data object. 
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

datatable(hour_data)
View(hour_data)

# This creates a bar chart with hours on the horizontal and totals on the vertical axes. 
# Labels given for both axes and a title supplied. Note, the bars have a solid steelblue
# fill. Code from Data Flair also demonstrated a red outline to the bars as a feature 
# of ggplot; I eliminated this option as the red outline contributes to visual clutter. 
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


# This creates a month_hour object from data_2014. Data is organized first by month, 
# and then by the hour of the day for each month. 
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

datatable(month_hour)
View(month_hour)

# This creates a bar group like before, with hour of the day on the horizontal axis
# and total number of trips on the vertical axis, but the bars are differentiated 
# by the months. 
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# This creates a day_group object from data_2014. 
# Plotting data by trips during every day of the month
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)
View(day_group)

# This plots a bar graph with day of the month on the horizontal
# and totals per day of the month on the vertical. 
ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# This creates a day_month_group object that further stratifies the prior
# plot. In this plot, the days of the month are subdivided into the months. 
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


# This creates a month_weekday object from data_2014. The bar graph
# puts a day of the week (by month) on the horizontal and totals on the vertical.
# I left this graph in as an example of visual clutter. It is extremely difficult
# to pick out a day of the week and follow it across all of the months, given 
# 7 colors. Overall, there is a trend of more Uber trips as the months go along. 
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day of the Week and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


# This creates a month_group object from data_2014. This bar group
# appears in a different order in the Data Flair project page. 
# This is a preferable way to display the monthly data as compared to the last
# graph that tried to mix in day of the week as well. 
# From this bar group, we can see that the total number of Uber trips in New York City
# has increased from April to September. By heights of the bars, the biggest increase 
# in Uber rides occurred in the month of September. 
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


# This section creates visualizations using the bases. Overall, there are 5 bases. 
# Base B02617 had the most number of Uber trips followed by B02512. 
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")


# This plot has bases on the horizontal, separated by month. 
# Base 02617 had the most number of Uber trips in the months 
# of July, August, and September as compared to all of the other months
# of all the other bases. 
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

# This plot has the trips per base separated by day of the week. 
# For 4 out of the 5 bases, Thursday is the most popular day of the week
# for an Uber, followed by Friday. 
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)


# This section creates a heatmap using ggplot. While I will create them below
# and they are colorful, I find them a little difficult to read.
# Using this legend, the lighter the color, the more Uber trips. 
# Overall, it appears that the lightest colors are around hours 16-18, or
# 4-6 pm. This probably corresponds to many folks leaving work at that time. 
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")


# Heatmap by Month and Day
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

#Heatmap by Month and Day of the Week. 
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

# Heatmap by Month and Bases
month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

# Heatmap by Bases and Day of the Week. 
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

# This section created a map visualization of the rides in New York with a geo-plot. 
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE") 



