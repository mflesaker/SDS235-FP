
library(shiny)
library(tidyverse)
library(scales)
library(bslib)
library(rsconnect)

raw_data <- read_csv("data.csv")

## code copied and modified from https://mastering-shiny.org/basic-ui.html (selectInput, plotOutput
## idea/syntax)
## and https://mastering-shiny.org/action-layout.html (titlePanel, sidebarLayout, sidebarPanel,
## mainPanel idea/syntax)
## and https://mastering-shiny.org/action-layout.html (tabPanel idea/syntax)
## and https://shiny.rstudio.com/articles/layout-guide.html (navbarPage idea/syntax and
## fluidRow, column idea and syntax)
## and https://shiny.rstudio.com/reference/shiny/1.6.0/textOutput.html (textOutput  idea/syntax)
## and https://campus.datacamp.com/courses/case-studies-building-web-applications-with-shiny-in-r/shiny-review?ex=3
## (strong("text") idea and syntax)

ui <- fluidPage(
  navbarPage(
    "CSC/SDS 235 Final Project: Michelle, Lauryn, Grace",
    tabPanel(
      "Static Data Analysis",
      fluidRow(
        column(
          5,
          strong("About Our Project"),
          textOutput("texta2")
        ),
        column(
          7,
          strong("Interesting Findings"),
          textOutput("textb"),
          plotOutput("static_plot")
        )
      ),
      fluidRow(
        column(
          5,
          strong("Characterizing the Sample"),
          textOutput("textc")
        ),
        column(
          7,
          plotOutput("static_plot2")
        )
      )
    ),
    tabPanel(
      "Interactive Dashboard",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "variable1", label = "Choose Variable 1", names(raw_data)),

          ## code for this conditional panel is directly copied and pasted from
          ## the example at https://shiny.rstudio.com/reference/shiny/1.3.0/conditionalPanel.html -----------

          selectInput(inputId = "plotType", label = "Plot Type", c(Bar = "bar", Count = "count")),
          # Only show this panel if the plot type is a two-way count
          conditionalPanel(
            condition = "input.plotType == 'count'",
            selectInput(inputId = "variable2", label = "Choose Variable 2", names(raw_data)),
          )
        ),
        
        ### ---------------------------------------------------------------------------------------
        
        ## the conditional plot code is based on the conditional panel code above
        mainPanel(
          conditionalPanel(
            condition = "input.plotType == 'bar'",
            plotOutput("plotbar")
          ),
          conditionalPanel(
            condition = "input.plotType == 'count'",
            plotOutput("plotcount")
          )
        )
      )
    )
  )
)



## code copied and modified from https://mastering-shiny.org/basic-app.html and
# https://mastering-shiny.org/basic-ui.html
server <- function(input, output, session) {
  output$plotbar <- renderPlot(
    ggplot(raw_data, aes(y = get(input$variable1))) +
      geom_bar() +
      ylab(input$variable1)
  )

  output$plotcount <- renderPlot(
    ggplot(raw_data, aes(x = get(input$variable1), y = get(input$variable2))) +

      ## geom_count idea and syntax from
      ## http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
      geom_count() +

      ## adding proper x and y labels from
      ## https://web.stanford.edu/~cengel/cgi-bin/anthrospace/building-my-first-shiny-application-with-ggplot
      xlab(input$variable1) +
      ylab(input$variable2)
  )

  output$static_plot <- renderPlot(
    ggplot(raw_data, aes(x = MARITAL_W56, y = F_PARTY_FINAL)) +
      geom_count() +
      ggtitle("Insert More Interesting Graph Here")
  )

  output$static_plot2 <- renderPlot(
    ggplot(raw_data, aes(x = MARITAL_W56, y = F_INCOME)) +
      geom_count() +
      ggtitle("Insert More Interesting Graph Here")
  )

  output$texta2 <- renderText("This application provides an analysis of and means to interact with data from the 2019 Pew Research Center survey on the intersection between romantic relationships and technology. The set of participants recruited for the survey, part of the American Trends Panel, were designed to serve as a representative sample of the US (Pew Research Center, 2019). Download the dataset with a Pew Research Center account and view their analysis here https://www.pewresearch.org/internet/2020/05/08/dating-and-relationships-in-the-digital-age/ (Vogels & Anderson, 2020).")
  output$textb <- renderText("Put what we find from the interesting findings as a summary here")
  output$textc <- renderText("This sample is largely married or living with a partner (__%), straight (__%), politically moderate (___%) or liberal (__%), non-Hispanic white (___%), and ages 30-64 (_%) with a college degree or higher (___%).")
}

# Run the application
shinyApp(ui = ui, server = server)
