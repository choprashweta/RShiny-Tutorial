#----------------------------#
#   PDSG: R Shiny Tutorial   #
#----------------------------#

#Set Working Directory
#setwd("~/Documents/PDSG/Shiny_Tutorial")

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

#Read dataset
dataset <- read.csv("Bike-Sharing-Dataset/hour.csv")


#Create outer skeleton of the app
# server<-function(input, output) {}
# ui <- fluidPage()
# shinyApp(ui = ui, server = server)


#The server takes an input object, and spits out an output object 
#that can then be used by the UI
server<-function(input, output) {}

#The entire UI is a fluidPage object
ui <- fluidPage()

#Essentially an object that has a ui and a server
shinyApp(ui = ui, server = server)


