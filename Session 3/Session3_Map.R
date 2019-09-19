#------------------------------------------
# R Shiny Tutorial - Leaflet Map
#-----------------------------------------

# Put your library here
library(shinythemes)
library(shiny)
library(foreign)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)




# Begin your code here
server <- shinyServer(function(input, output) {
  
  #-------------------------
  # Maps
  #-------------------------
  
  # Use these coordinates to center the map
  lat <- 39.1582
  lng <- -75.5244
  zoom <- 8
  
  #reading in data 
  place_data <- read.csv("place_data.csv")
  View(place_data)
  
  #reading  in the shapefile
  places <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly",encoding = "UTF-8")
  View(places@data)
  
  # using a left_join because the datafile and the shapefile line up EXACTLY - if they dont, you cant use left join
  places@data <- merge(places@data, place_data, by = "CODE")
  
  #Create bins
  places$Risk_bins <- ifelse(places$Risk <= 24, "0-24", ifelse(places$Risk <= 49, "25-49", ifelse(places$Risk <= 74, "50-74", "75-98")) )
  
  
  # This makes a pop up with information from our data
  place_popup <- paste0("<strong>Zip Code: </strong>", 
                        places$CODE, 
                        "<br> ",
                        "<br> ",
                        "<strong>Risk Index: </strong>",
                        places$Risk,
                        "%",
                        "<br> ",
                        "<strong>Poverty: </strong>",
                        places$Poverty,
                        "%",
                        "<br> ",
                        "<strong>Education: </strong>",
                        places$Education,
                        "%",
                        "<br> ",
                        "<strong>Unemployment: </strong>",
                        places$Unemployment,
                        "%",
                        "<br> ",
                        "<strong>Crime: </strong>", 
                        places$Crime,
                        "%",
                        "<br> ",
                        "<strong>ACEs: </strong>", 
                        places$ACEs,
                        "%"
  )
  
  #Create colour scheme
  factpal <- colorFactor(c("#FBDA93", "#F69953",
                           "#DF5258",
                           "#C42834"), places$Risk_bins)
  
  # Here's our map! We put it in a map object
  map <- leaflet(places) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .8,
                color = factpal(places$Risk_bins),
                weight = 1, 
                popup = place_popup) %>%
    addLegend("bottomright", 
              colors =c("#FBDA93", "#F69953","#DF5258", "#C42834"),
              labels= c("0-24", "25-49","50-74","75-98"),  
              title= "Risk: Lowest to Highest",
              opacity = 1) 
  
  
  # output the map
  output$map <- renderLeaflet(map)
  
  
  
  
}) 

# Begin your code here, don't forget to comment it!
ui <- shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  # Set the style for the header
                  tags$head(
                    tags$style("h2 {color: #ee5500; }
                                                   h1 {color: #C42834}; }
                                                   ")),
                  # Create a title
                  headerPanel("Mapping Risk Index in Philadelphia by Zip Code"),
                  br(), # br() is borrowed from html. It creates a space, called a break
                  h2(""), # Another way to create a space is to add an empty text space. If there were 
                  # text inside those parenthesis, it would be printed in the app. Try it!
                  
                  # This line controls the size of the map. I have set the width to 100% - this will
                  # adjust the map to the size of any screen it is displayed on. 
                  # The height is measured in px because I do want to control that length 
                  # Most importantly, notice "map" is coming from the server script
                  leafletOutput("map", width = "100%", height = "600px")
                  
                  
))

shinyApp(ui = ui, server = server)
