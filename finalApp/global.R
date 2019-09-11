#----------------------------#
#   PDSG: R Shiny Tutorial   #
#----------------------------#

#Set Working Directory
#setwd("D:/UPenn/RShiny_Tutorial/rshinytutorialmaterial")

for (packageInApp in packrat:::appDependencies()) {
  if (!(packageInApp %in% rownames(installed.packages()))) {
    install.packages(packageInApp)
  }
}
rm(packageInApp)

#Import Packages (if not installed use install.packages("package_name"))
library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library(shinydashboard)
library(data.table)
library(stargazer)


# Read dataset
dataset <- read.csv("data/Bike-Sharing-Dataset/hour.csv")

# Change season to factor
dataset$season <- factor(
  dataset$season,
  levels = c(1, 2, 3, 4),
  labels = c("spring", "summer", "fall", "winter")
)

# Create date fields as factors so we can have labels
dataset$mnth <- factor(
  dataset$mnth,
  levels = c(1:12),
  # labels = c("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  labels = month.abb # month.abb is default built-in variable in R
)
dataset$weekday <- factor(
  dataset$weekday,
  levels = c(0:6),
  # labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat")
  labels = weekdays(x=as.Date(seq(7), origin="2019-09-07"), abbreviate = T)
)
dataset$weathersit <- factor(
  dataset$weathersit,
  levels = c(1:4),
  labels = c("Clear", "Cloudy", "Snow", "Heavy Rain")
)
dataset$dteday <- as.Date(dataset$dteday)
