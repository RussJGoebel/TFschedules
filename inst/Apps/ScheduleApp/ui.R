ui <- fluidPage(

  # Application title
  titlePanel("Teaching Fellow Schedule Finder"),



  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("button", "Add Row"),
      actionButton("button2", "Remove Row")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Schedule Input",rhandsontable::rHandsontableOutput("hot")),
        tabPanel("Summary of Schedule Conflicts",tableOutput("Summary")),
        tabPanel("GRS",tableOutput("GRS")),
        tabPanel("CAS",tableOutput("CAS"))
      )
    )
  )
)
