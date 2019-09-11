shinyServer(function(input, output, session){
  
  output$seasonPlot <- renderPlot({
    # Group data by season
    users_by_season <- dataset %>%
      group_by(season) %>%
      summarize(
        casual_count = sum(casual),
        registered_count = sum(registered),
        total_count = sum(cnt)
      )
    
    # Change to dataframe
    users_by_season <- as.data.frame(users_by_season)
    
    plot_data <- melt(
      users_by_season[, c('season',
                          'casual_count',
                          'registered_count',
                          'total_count')], 
      id.vars = 1)
    
    # Create a triple bar chart
    season_plot <- ggplot(data = plot_data) +
      geom_bar(aes(x = season, y = value, fill = variable),
               stat = "identity",
               position = "dodge") +
      ggtitle("Bike Sharing Count by Season") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14)) +
      scale_fill_brewer(palette = "PuBu")
    season_plot
  })
  
  # Deriving filtered data for Visualizations
  plotDataContent <- reactive({
    sub <- dataset
    if (input$season_viz != "All") {
      sub <- sub[sub$season == input$season_viz, ]
    }
    
    if (input$weekday_viz != "All") {
      sub <- sub[sub$weekday == input$weekday_viz, ]
    }
    
    if (input$weathersit_viz != "All") {
      sub <- sub[sub$weathersit == input$weathersit_viz, ]
    }
    sub <- sub[(sub$hum > input$hum_viz[1]) &
                 (sub$hum < input$hum_viz[2]), ]
    sub <- sub[(sub$windspeed > input$windspeed_viz[1]) &
                 (sub$windspeed < input$windspeed_viz[2]), ]
    sub <- sub[(sub$temp > input$temp_viz[1]) &
                 (sub$temp < input$temp_viz[2]), ]
    sub
  })
  
  #### Heatmap of Usage ----
  # Assign colors for our map
  
  output$timePlot1 <- renderPlot({
    dayHour <- plotDataContent() %>%
      group_by(hr, weekday) %>%
      summarise(
        Cas = sum(casual),
        Reg = sum(registered),
        Total = sum(cnt)
      )
    
    col1 = "#a8d8e7"
    col2 = "#1a6f89"
    
    ggplot(dayHour, aes(hr, weekday)) + geom_tile(aes(fill = Total), colour = "white", na.rm = TRUE) +
      scale_fill_gradient(low = col1, high = col2) +
      guides(fill = guide_legend(title = "Total Bike Sharing Users")) +
      theme_bw() + theme_minimal() +
      labs(title = "Heatmap of Bike Sharing Users by Day of Week and Hour of Day",
           x = "Hour of Day", y = "Day of Week") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  #### Users over Time ----
  output$timePlot2 <- renderPlot({
    df <- plotDataContent() %>%
      group_by(dteday) %>%
      summarise(
        Cas = sum(casual),
        Reg = sum(registered),
        Total = sum(cnt)
      ) %>%
      select(dteday, Cas, Reg, Total) %>%
      gather(key = "riders", value = "value", -dteday)
    
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
  
  #### Data Table ----
  dataTableContent <- reactive({
    sub <- dataset
    if (input$season != "All") {
      sub <- sub[sub$season == input$season, ]
    }
    if (input$weekday != "All") {
      sub <- sub[sub$weekday == input$weekday, ]
    }
    if (input$weathersit != "All") {
      sub <- sub[sub$weathersit == input$weathersit, ]
    }
    sub <- sub[(sub$hum > input$hum[1]) &
                 (sub$hum < input$hum[2]), ]
    sub = sub[(sub$windspeed > input$windspeed[1]) &
                (sub$windspeed < input$windspeed[2]), ]
    sub = sub[(sub$temp > input$temp[1]) &
                (sub$temp < input$temp[2]), ]
    sub = sub[(sub$dteday >= as.Date(input$dteday[1])) &
                (sub$dteday <= as.Date(input$dteday[2])), ]
    sub
  })
  
  
  output$datatable <- DT::renderDataTable(
    DT::datatable({
      # Choosing just a few columns to display (leaving off timestamp)
      d = dataTableContent()
      d
    })
  )
  
  #### Regression ----
  # Season and Holiday should be categorical variables
  dataset$season <- factor(
    dataset$season,
    labels = c("spring", "summer", "fall", "winter")
  )
  dataset$holiday <- factor(
    dataset$holiday,
    labels = c("Working Day", "Holiday")
  )
  
  # creating a reactive regression forumula that uses inputs from the check list
  # as independent variables to predict the variable ACE_BI
  # then, put that formula into the svyglm() from survey package which outputs 
  # a weighted regression
  model <- reactive({
    regFormula <- as.formula(paste(input$dv," ~ ",paste(input$iv,collapse="+")))
    lm(regFormula, dataset)
  })
  
  # Creating pretty labels for the stargazer table
  # Here, we are using if statements to build a vector, covars, that is dependent on the inputs
  #from the beck list. 
  covar.label <- reactive({
    covars <-c()
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
  
  # Create nice regression table output
  # stargazer() comes from the stargazer package
  output$regTab <- renderText({
    covars <- covar.label()
    dep <- dep.label()
    stargazer(model(),type="html",dep.var.labels = dep,covariate.labels = covars)
  })
  
  #### Users by Group ----
  # Create a triple bar chart
  output$groupPlot <- renderPlot({
    subset <- dataset %>%
      group_by_(input$x_axis) %>%
      summarize(
        casual_count = sum(casual),
        registered_count = sum(registered),
        total_count = sum(cnt)
      )
    subset <- as.data.frame(subset)
    plot_data <- melt(
      subset[, c(input$x_axis,
                 'casual_count',
                 'registered_count',
                 'total_count')], 
      id.vars = 1)
    
    
    # Make the plot a reactive element
    plot <- 
      ggplot(data = plot_data) +
      geom_bar(
        aes_string(x = input$x_axis, y = "value", fill = "variable"),
        stat = "identity",
        position = "dodge") +
      ggtitle("Bike Sharing Counts") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)
      ) +
      scale_fill_brewer(
        palette = "PuBu", 
        labels = c("Casual Count",
                   "Registered Count",
                   "Total Count")
      )
    return(plot)
  })
})
