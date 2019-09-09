#### Shiny UI ----
# The entire UI is a fluidPage object
ui <- fluidPage(
  #We can give it a theme
  theme = shinytheme("journal"),
  # The main app - Navigation Bar page
  navbarPage(
    # Give it a title
    "PDSG Tutorial: Bike Sharing",
    # Now within this we may create different tabs using tabPanels
    tabPanel(
      # Give the Tab Panel a Header
      "Welcome",
      fluidRow(
        # A fluid row has a total width of 12
        column(
          width = 3,
          br(),
          br(),
          br(),
          br(),
          # Adding the Title of the page
          h1("PDSG Shiny Tutorial: "),
          h2("Bike Sharing")
        ),
        column(
          width = 9,
          # Load an image from the www folder
          img(src = "BikeSharingImage.png", width = "100%")
        ),
        # The entire row is colored light grey
        style = "background-color: #D3D3D3;"
      )
    ),
    tabPanel(
      # Give the Tab Panel a Header
      "Time Patterns",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          selectInput(
            "x_axis", "Select an element for the x axis", 
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
      "Data Table",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tabsetPanel(
            tabPanel(
              "Vars",
              wellPanel(
                sliderInput(
                  "windspeed",
                  "Windspeed",
                  min = 0,
                  max = 1.0,
                  val = c(0.1, 0.25)
                ),
                selectInput(
                  "weekday", "Weekday", choices =
                    c("All", levels(dataset$windspeed))
                ),
                selectInput(
                  "season", "Season", choices =
                    c("All", levels(dataset$season))
                ),
                selectInput(
                  "weathersit", "Weather", choices =
                    c("All", levels(dataset$weathersit))
                ),
                sliderInput(
                  "temp",
                  "Tempearture",
                  min = 0,
                  max = 1.0,
                  val = c(0.5, 0.8)
                ),
                sliderInput(
                  "hum",
                  "Humidity",
                  min = 0,
                  max = 1.0,
                  val = c(0.5, 0.8)
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
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Raw Data",
              shinydashboard::box(
                title = "Filtered Data",
                width = NULL,
                status = "primary",
                div(style = 'overflow-x: scroll', DT::dataTableOutput("datatable"))
              )
            )
          )
        )
      )
    ),
    tabPanel(
      # Header for the Panel
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
            # Note cnt is selected when the app starts
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
            # Note season is selected when the app starts
          )
        ),
        mainPanel(
          h3("Table of Regression Coefficients"),
          tableOutput("regTab")
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
            val = c(0.0, 1.0)
          ),
          selectInput(
            "weekday_viz", "Weekday", choices =
              c("All", levels(dataset$windspeed))
          ),
          selectInput(
            "season_viz", "Season", choices =
              c("All", levels(dataset$season))
          ),
          
          selectInput(
            "weathersit_viz", "Weather", choices =
              c("All", levels(dataset$weathersit))
          ),
          
          sliderInput(
            "temp_viz",
            "Tempearture",
            min = 0,
            max = 1.0,
            val = c(0.0, 1.0)
          ),
          sliderInput(
            "hum_viz",
            "Humidity",
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