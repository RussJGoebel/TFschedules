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
        tabPanel("Schedule Input",actionButton("button1", "Add Row"),
                 actionButton("button2", "Remove Row"),
                 rhandsontable::rHandsontableOutput("hot", width = "100%"),
                 p("In the `Schedule Input` tab, for each weekly activity you would like to schedule around,
                   use `Add Row` to create a row for that activity. Check the boxes corresponding to the days that this activity meets. Then, input the Start time and Stop time of the activity.")),
        tabPanel("Summary of Schedule Conflicts",dataTableOutput("Summary"),
                 p("In the Summary of Schedule Conflicts tab, the number of conflicting sections for each course letter is summarized by section type (e.g, CAS MA415 A is one letter,
                   which may have a lecture sections CAS MA415 A1, a discussion CASMA415 A2, etc). Here, '4/5' would mean 4 of the 5 sections are compatible with your schedule.")),
        tabPanel("CAS",dataTableOutput("CAS"),
                 p("Sections with schedule conflicts are highlighted in gray.")),
        tabPanel("GRS",dataTableOutput("GRS"),
                 p("Sections with schedule conflicts are highlighted in gray.")),
        tabPanel("Notes",
                 h3("Please double check to make sure that there are no schedule conflicts before making decisions using this app."),
                 h3("Remember to close your browser after you are finished. This app can be hosted for only 25 active hours a month!"),
                 HTML("<p>Code for this app can be downloaded at <a href='https://github.com/RussJGoebel/TFschedules'> this Github repository.</a></p>"))

      )
    )
  )
)
