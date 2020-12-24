library(shinyTime)
library(TFschedules)
library(DT)

ui <- fluidPage(


  tags$head(tags$style(HTML('* {font-family: BentonSans Book;}'))),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(10, offset = 1,
           # Application title
           titlePanel("Teaching Fellow Schedule Helper")
    ),




    # Show a plot of the generated distribution
    column(10,offset = 1,
      tabsetPanel(
        tabPanel("Schedule Input",actionButton("button1", "Add Row"),actionButton("button2", "Remove Row"),rhandsontable::rHandsontableOutput("hot")),
        tabPanel("Summary of Schedule Conflicts",dataTableOutput("Summary")),
        tabPanel("GRS",dataTableOutput("GRS")),
        tabPanel("CAS",dataTableOutput("CAS"))
      )
    )
  )
)
