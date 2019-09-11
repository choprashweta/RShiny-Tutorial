fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    title = "PDSG Tutorial: Bike Sharing",
    tabPanel(
      title = "Welcome",
      fluidRow(
        column(
          width = 3,
          rep(br(), 4), # 4 new empty lines, to generate whitespace for visual purpose
          h1("PDSG Shiny Tutorial: "),
          h2("Bike Sharing")
        ),
        column(
          width = 9,
          img(src = "BikeSharingImage.png", width = "100%")
        ),
        style = "background-color: #D3D3D3;"
      )
    ),
    tabPanel(
      title = "Time Patterns",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          selectInput(
            inputId = "x_axis", 
            label = "Select an element for the x axis", 
            choices =
              list(
                "Season" = "season",
                "Holiday" = "holiday",
                "Month" = "mnth",
                "Hour" = "hr",
                "Weekday" = "weekday"
              )
          )
        ),
        mainPanel(
          plotOutput("groupPlot", height = "400px")
        )
      )
    ),
    tabPanel(
      title = "Dual Visualization",
      sidebarLayout(
        position = "left",
        sidebarPanel(
          h2("Build your model"),
          sliderInput(
            inputId = "windspeed_viz",
            label = "Windspeed",
            min = 0,
            max = 1.0,
            val = c(0.0, 1.0)
          ),
          selectInput(
            inputId = "weekday_viz", 
            label = "Weekday", 
            choices =
              c("All", levels(dataset$windspeed))
          ),
          selectInput(
            inputId = "season_viz",
            label ="Season", 
            choices =
              c("All", levels(dataset$season))
          ),
          selectInput(
            inputId = "weathersit_viz", 
            label = "Weather",
            choices =
              c("All", levels(dataset$weathersit))
          ),
          sliderInput(
            inputId = "temp_viz",
            label = "Tempearture",
            min = 0,
            max = 1.0,
            val = c(0.0, 1.0)
          ),
          sliderInput(
            inputId = "hum_viz",
            label = "Humidity",
            min = 0,
            max = 1.0,
            val = c(0.0, 1.0)
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