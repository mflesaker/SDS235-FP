
library(shiny)
library(tidyverse)
library(scales)

raw_data <- read_csv("data.csv")

## code copied and modified from https://mastering-shiny.org/basic-ui.html (selectInput, plotOutput
## idea/syntax)
## and https://mastering-shiny.org/action-layout.html (titlePanel, sidebarLayout, sidebarPanel,
## mainPanel idea/syntax)

ui <- fluidPage(
  titlePanel("CSC/SDS 235 Final Project: Michelle, Lauryn, Grace"),
  tabsetPanel(
    tabPanel("Interactive Dashboard",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "variable1", label = "Choose Variable 1", names(raw_data)),
          selectInput(inputId = "variable2", label = "Choose Variable 2", names(raw_data))
        ),
        mainPanel(
          plotOutput("plot")
        )
      )
    ),
    tabPanel("Static Data Analysis",
        verbatimTextOutput("Abour Our Project")
    )
  )
)


## code copied and modified from https://mastering-shiny.org/basic-app.html and
# https://mastering-shiny.org/basic-ui.html
server <- function(input, output, session) {
  output$plot <- renderPlot(
    ggplot(raw_data, aes(x = get(input$variable1), y = get(input$variable2))) +

      ## geom_count idea and syntax from
      ## http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
      geom_count() +

      ## adding proper x and y labels
      ## https://web.stanford.edu/~cengel/cgi-bin/anthrospace/building-my-first-shiny-application-with-ggplot
      xlab(input$variable1) +
      ylab(input$variable2)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
