#----------------------------#
#   PDSG: R Shiny Tutorial   #
#----------------------------#

#Set Working Directory
#setwd("D:/UPenn/RShiny_Tutorial/rshinytutorialmaterial")

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
dataset <- read.csv("data/hour.csv")

#Change season to factor
dataset$season <- factor(
  dataset$season,
  levels = c(1, 2, 3, 4),
  labels = c("spring", "summer", "fall", "winter")
)

#Create date fields as factors so we can have labels
dataset$mnth <- factor(
  dataset$mnth,
  levels = c(1:12),
  labels = c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
)
dataset$weekday <- factor(
  dataset$weekday,
  levels = c(0:6),
  labels = c("Sun", "Mon", "Tue", "Wed",
             "Thur", "Fri", "Sat")
)


dataset$weathersit <- factor(
  dataset$weathersit,
  levels = c(1:4),
  labels = c("Clear", "Cloudy", "Snow", "Heavy Rain")
)

dataset$dteday = as.Date(dataset$dteday)

#Season and Holiday should be categorical variables
dataset$holiday <- factor(dataset$holiday,
                          labels = c("Non-holiday", "Holiday"))

#Create outer skeleton of the app
# server<-function(input, output) {}
# ui <- fluidPage()
# shinyApp(ui = ui, server = server)

server <- function(input, output) {
  
  ### Deriving filtered data for Visualizations
  
  
  #--------------------#
  #   Heatmap of Usage #
  #--------------------#
  
  #Assign colors for our map
  col1 = "#a8d8e7"
  col2 = "#1a6f89"
  
  
  output$timePlot1 <- renderPlot({
    
    dayHour <- dataset %>%
      group_by(hr, weekday) %>%
      summarise(
        Cas = sum(casual),
        Reg = sum(registered),
        Total = sum(cnt))
    
    ggplot(dayHour, aes(hr, weekday)) + geom_tile(aes(fill = Total), colour = "white", na.rm = TRUE) +
      scale_fill_gradient(low = col1, high = col2) +
      guides(fill = guide_legend(title = "Total Bike Sharing Users")) +
      theme_bw() + theme_minimal() +
      labs(title = "Heatmap of Bike Sharing Users by Day of Week and Hour of Day",
           x = "Hour of Day", y = "Day of Week") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
  
  #-----------------#
  # Users over Time #
  #-----------------#
  
  
  output$timePlot2 <- renderPlot({
    
    df <- dataset %>%
      group_by(dteday) %>%
      summarise(
        Cas = sum(casual),
        Reg = sum(registered),
        Total = sum(cnt)
      ) %>%
      select(dteday, Cas, Reg, Total) %>%
      gather(key = "riders", value = "value",-dteday)
    
    
    ggplot(df, aes(x = dteday, y = value)) +
      geom_line(aes(color = riders), size = 0.2) +
      scale_color_manual(values = c("#3f78bf", "#bf73cc", "#1ebbd7")) +
      theme_minimal() +
      xlab("Date") +
      ylab("Number of Riders") +
      ggtitle("Time Series Plot") +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14))
    
  })
  
  
  
  #-----------------#
  # Users by Group #
  #-----------------#
  
  #Create a triple bar chart
  output$groupPlot <- renderPlot({
    
    subset <- dataset %>%
      group_by_(input$x_axis) %>%
      summarize(
        casual_count = sum(casual),
        registered_count = sum(registered),
        total_count = sum(cnt)
      )%>%
      gather(key = 'variable', value = 'value', -1)
    
    #subset <- as.data.frame(subset)
    
    #Make the plot a reactive element
    plot <- ggplot(data = subset) +
      #aes_string
      geom_bar(aes_string(x = input$x_axis, y = "value", fill = "variable"),
               stat = "identity",
               position = "dodge") +
      ggtitle("Bike Sharing Counts") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14)) +
      scale_fill_brewer(palette = "PuBu", 
                        labels = c("Casual Count",
                                   "Registered Count",
                                   "Total Count"))
    
    
    return(plot)
    
  }) 
  
}

##############################
######### UI #################
##############################


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
        column(width = 9,
               #Load an image from the www folder
               img(src = "BikeSharingImage.png", width = "100%")),
        #The entire row is colored light grey
        style = "background-color: #D3D3D3;"
        
        
      )
      
    ),
    tabPanel(
      #Give the Tab Panel a Header
      "Time Patterns",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          selectInput("x_axis", "Select an element for the x axis", 
                      choices =
                        list(
                          "Season" = "season",
                          "Holiday" = "holiday",
                          "Month" = "mnth",
                          "Hour" = "hr",
                          "Weekday" = "weekday"
                          
                        ))
        ),
        mainPanel(
          plotOutput("groupPlot", height = "400px")
          
          
        )
      )
    ),
    
    tabPanel("Data Table",
             
             sidebarLayout(
               sidebarPanel(width = 3,
                            tabsetPanel(
                              tabPanel(
                                "Vars",
                                wellPanel(
                                  sliderInput(
                                    "windspeed",
                                    "Windspeed",
                                    min = 0,
                                    max = 1.0,
                                    value = c(0.1, 0.25)
                                  ),
                                  selectInput("weekday", "Weekday", choices =
                                                c("All", levels(dataset$weekday))),
                                  selectInput("season", "Season", choices =
                                                c("All", levels(dataset$season))),
                                  
                                  selectInput("weathersit", "Weather", choices =
                                                c("All", levels(dataset$weathersit))),
                                  
                                  sliderInput(
                                    "temp",
                                    "Temperature",
                                    min = 0,
                                    max = 1.0,
                                    value = c(0.5, 0.8)
                                  ),
                                  
                                  sliderInput(
                                    "hum",
                                    "Humidity",
                                    min = 0,
                                    max = 1.0,
                                    value = c(0.5, 0.8)
                                  ),
                                  
                                  dateRangeInput(
                                    "dteday",
                                    "Date Range",
                                    start = "2011-01-01",
                                    end = "2012-12-31",
                                    min = "2011-01-01",
                                    max = "2012-12-31"
                                  )
                                )
                              )
                            )),
               
               mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel(
                             "Raw Data"
                             
                           )
                         ))
               
             )),
    tabPanel(
      
      #Header for the Panel
      "Modeling Usage",
      headerPanel("Usage Prediction Model"),
      # The sidebar contains the option for the end user to select
      # multiple independent variables and one dependent variable
      sidebarLayout(
        position = "right",
        sidebarPanel(
          h2("Build your model"),
          br(),
          h4("Select One Dependent Variable:"),
          radioButtons(
            "dv",
            label = NULL,
            choices = list(
              "Casual Users" = "casual",
              "Registered Users" = "registered",
              "Total Users" = "cnt"
            ),
            selected = "cnt"
            #Note cnt is selected when the app starts
          ),
          h4("Select Multiple Independent Variables:"),
          checkboxGroupInput(
            "iv",
            label = NULL,
            choices = list(
              "Season" = "season",
              "Holiday" = "holiday",
              "Average Temperature" = "atemp",
              "Humidity" = "hum",
              "Windspeed" =
                "windspeed"
            ),
            selected = "season"
            #Note season is selected when the app starts
          )
        ),
        
        mainPanel(
          h3("Table of Regression Coefficients")
          
        )
      )
      
      
    ),
    tabPanel(
      "Dual Visualization",
      
      sidebarLayout(
        
        position = "left",
        sidebarPanel(
          h2("Build your model"),
          
          sliderInput(
            "windspeed_viz",
            "Windspeed",
            min = 0,
            max = 1.0,
            value = c(0.0, 1.0)
          ),
          selectInput("weekday_viz", "Weekday", choices =
                        c("All", levels(dataset$weekday))),
          selectInput("season_viz", "Season", choices =
                        c("All", levels(dataset$season))),
          
          selectInput("weathersit_viz", "Weather", choices =
                        c("All", levels(dataset$weathersit))),
          
          sliderInput(
            "temp_viz",
            "Temperature",
            min = 0,
            max = 1.0,
            value = c(0.0, 1.0)
          ),
          
          sliderInput(
            "hum_viz",
            "Humidity",
            min = 0,
            max = 1.0,
            value = c(0.0, 1.0)
          )
          
        ),
        
        mainPanel(
          h3("Visualizations"),
          
          plotOutput("timePlot1"),
          
          br(),
          
          plotOutput("timePlot2")
          
        )
        
        
      )
    )
    
    
  )
  
  
)

shinyApp(ui = ui, server = server)
