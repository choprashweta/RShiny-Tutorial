#----------------------------#
#   PDSG: R Shiny Tutorial   #
#----------------------------#

#Set Working Directory
setwd("~/Documents/PDSG/Shiny_Tutorial")

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




server<-function(input, output) {}

#The entire UI is a fluidPage object
ui <- fluidPage(
  #We can give it a theme
  theme = shinytheme("journal"),
  
  #The main app - Navigation Bar page
  navbarPage(
    #Give it a title
    "PDSG Tutorial: Bike Sharing",
    
    #Now within this we may create different tabs using tabPanels
    tabPanel(
      #Give the Tab Panel a Header
      "Welcome",
      fluidRow(
        #A fluid row has a total width of 12
        column(
        width = 3,
        br(),
        br(),
        br(),
        br(),
        #Adding the Title of the page
        h1("PDSG Shiny Tutorial: "),
        h2("Bike Sharing")
      ),
        column(
        width = 9,
        #Load an image from the www folder
        img(src = "BikeSharingImage.png", width = "100%"
      )),
      #The entire row is colored light grey
      style = "background-color: #D3D3D3;"
      
      
      )
      
    ),
    tabPanel(
      #Give the Tab Panel a Header
      "Time Patterns"
    )
    
  )
  
  
)

shinyApp(ui = ui, server = server)


