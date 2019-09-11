# Edit this file for part 1
# See ui_template.R for comments / help
# see ui_sol.R for solution

fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    title = "My Shiny App ",
    tabPanel(
      title = "First",
      fluidRow(
        column(
          width = 3,
          # -- contents
        ),
        column(
          width = 9,
          # -- contents
        ),
        style = "background-color: #D3D3D3;"
      )
    ),
    tabPanel(
      title = "Second",
      sidebarLayout(
        position = "right",
        sidebarPanel(
          # -- contents
        ),
        mainPanel(
          # -- contents
        )
      )
    )
  )
)