fluidPage(
  # We can give it a theme
  theme = shinytheme("journal"),
  # The main app - Navigation Bar page
  navbarPage(
    # Give it a title
    title = "My Shiny APp ",
    # Create multiple tabs by "listing" R object tabPanel(...).
    # "listing" is in quotes because you do not need to specify an actual R list like list(...)
    # simply separate each tabPanel by a comma like: tabPanel(...), tabPanel(...), ...
    tabPanel(
      # Give the first panel a title. By default, Shiny will open with the first tab
      title = "First",
      # The first tab will use row & column structure
      fluidRow(
        # A fluid row has a total width of 12. Any width > 12 will "spill over" into the next row
        column(
          # Add contents for row 1, col 1-3 (width size)
          width = 3,
          # -- contents
        ),
        column(
          # Add contents for row 1, col 4-12 (12 - already defined columns + width size)
          width = 9,
          # -- contents
        ),
        # (optional) Apply style to row 1
        style = "background-color: #D3D3D3;"
        # Lack of comma on the line above signifies Shiny that I have finished defining
        # my row
      )
      # Lack of comma on the line above signifies Shiny that I have finished defining
      # the contents of "First" tab
    ), # <-- note this comma that separates first tabPanel(...) from the second tabPanel(...)
    tabPanel(
      # Give the second panel a title
      title = "Second",
      # The second tab will not use row & column structure. Instead, it will use sidebar & main area structure
      sidebarLayout(
        # specify the position of the sidebar relative to the main area. "left" or "right" only
        position = "right",
        sidebarPanel(
          # -- contents
        ),
        # Main area
        mainPanel(
          # -- contents
        )
      )
    ) # Add "," at the end of this line if you want to add more tab panels below.
    # Add more tabPanel(...) below
  )
)