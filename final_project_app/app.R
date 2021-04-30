#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)

raw_data <- read_csv("data.csv")

# Define UI for application that draws a histogram


## code copied and modified from https://mastering-shiny.org/basic-ui.html
ui <- fluidPage(
    selectInput(inputId = "variable1", label = "Choose Variable 1", names(raw_data)),
    selectInput(inputId = "variable2", label = "Choose Variable 2", names(raw_data)),
    plotOutput("plot")
)

# Define server logic required to draw a histogram

## code copied from https://mastering-shiny.org/basic-app.html
server <- function(input, output, session) {
    
    output$plot <- renderPlot(
        ggplot(raw_data, aes(x = get(input$variable1), y = get(input$variable2))) +
            geom_jitter()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
