############################
# SESSION 2: Helper Script #
############################

### HEATMAP & TIME-SERIES Reactivity ###

plotDataContent = reactive({
  
  sub <- dataset
  
  if (input$season_viz != "All") {
    sub = sub[sub$season == input$season_viz, ]
  }
  
  if (input$weekday_viz != "All") {
    sub = sub[sub$weekday == input$weekday_viz, ]
  }
  
  if (input$weathersit_viz != "All") {
    sub = sub[sub$weathersit == input$weathersit_viz, ]
  }
  
  
  sub = sub[(sub$hum > input$hum_viz[1]) &
              (sub$hum < input$hum_viz[2]), ]
  
  sub = sub[(sub$windspeed > input$windspeed_viz[1]) &
              (sub$windspeed < input$windspeed_viz[2]), ]
  
  sub = sub[(sub$temp > input$temp_viz[1]) &
              (sub$temp < input$temp_viz[2]), ]
  
  sub
  
})

##### REGRESSION MODEL ################

# Model code: lm(DependentVariable ~ IV1 + IV2 + IV3, DatasetName)
# UI inputs corresponding: DependentVariable (dv) and IV (iv)

#creating a reactive regression forumula that uses inputs from the check list
#as independent variables to predict the variable 
regFormula<- reactive({
  as.formula(paste(input$dv," ~ ",paste(input$iv,collapse="+"))) 
})

# then, put that formula into the lm() from survey package which outputs 
# a weighted regression
model <- reactive({
  lm(regFormula(), dataset)
})

# Creating pretty labels for the stargazer table
# Here, we are using if statements to build a vector, covars, that is dependent on the inputs
#from the beck list. 
covar.label <- reactive({
  covars<-c()
  if ('season' %in% input$iv){
    covars <- c(covars,"Summer", "Fall", "Winter")
  } 
  
  if ('holiday' %in% input$iv){
    covars <- c(covars,"Holiday")
  } 
  
  if ('atemp' %in% input$iv){
    covars <- c(covars,"Average Temperature")
  } 
  
  if ('hum' %in% input$iv){
    covars <- c(covars,"Humidity")
  } 
  
  if ('windspeed' %in% input$iv){
    covars <- c(covars,"Windspeed")
  } 
  return(covars)
})


dep.label <- reactive({
  dep <- ""
  if (input$dv == "casual"){
    dep <- "Casual Users"
  }
  
  else if (input$dv == "registered"){
    dep <- "Registered Users"
  }
  else {
    dep <- "Total Users"
  }
  
  return(dep)
})

#Create nice regression table output
#stargazer() comes from the stargazer package
output$regTab <- renderText({
  covars <- covar.label()
  dep <- dep.label()
  stargazer(model(),type="html",dep.var.labels = dep, covariate.labels = covars)
})  

#Add in UI tableOutput("regTab")


##### DATA TABLE ###########

#UI

shinydashboard::box(
  title = "Filtered Data",
  width = NULL,
  status = "primary",
  div(style = 'overflow-x: scroll', DT::dataTableOutput("datatable"))
)

# Server

output$datatable <- DT::renderDataTable(DT::datatable({
  
  d = dataset
  
  d
  
}))

### EXERCISE ###

#Replace dataset by a function that filters the data based on the UI inputs














### Answer

dataTableContent <- reactive({
  sub <- dataset
  
  if (input$season != "All") {
    sub = sub[sub$season == input$season, ]
  }
  
  if (input$weekday != "All") {
    sub = sub[sub$weekday == input$weekday, ]
  }
  
  if (input$weathersit != "All") {
    sub = sub[sub$weathersit == input$weathersit, ]
  }
  
  
  sub = sub[(sub$hum > input$hum[1]) &
              (sub$hum < input$hum[2]), ]
  
  sub = sub[(sub$windspeed > input$windspeed[1]) &
              (sub$windspeed < input$windspeed[2]), ]
  
  sub = sub[(sub$temp > input$temp[1]) &
              (sub$temp < input$temp[2]), ]
  
  sub = sub[(sub$dteday >= as.Date(input$dteday[1])) &
              (sub$dteday <= as.Date(input$dteday[2])), ]
  
  
  sub
  
  
  
})










