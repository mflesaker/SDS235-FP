library(shiny)
library(tidyverse)
library(scales)
library(bslib)
library(rsconnect)
library(shinythemes)
library(plotly)

raw_data <- read_csv("data.csv") %>%
  select(
    -QKEY, -INTERVIEW_START_W56, -INTERVIEW_END_W56, -DEVICE_TYPE_W56, -SAMPLE_W56, -FORM_W56, -WHYDATE10YRHARDOE_M1_W56, -WHYDATE10YRHARDOE_M2_W56, -WHYDATE10YRHARDOE_M3_W56, -WHYDATE10YRHARD_TECH_W56, -WHYDATE10YREASYOE_M1_W56, -WHYDATE10YREASYOE_M2_W56, -WHYDATE10YREASYOE_M3_W56, -WHYDATE10YREASY_TECH_W56, -ONIMPACTPOSOE_M1_W56, -ONIMPACTPOSOE_M2_W56,
    -ONIMPACTPOSOE_M3_W56, -ONIMPACTNEGOE_M1_W56, -ONIMPACTNEGOE_M2_W56,
    -ONIMPACTNEGOE_M3_W56, -F_ACSWEB, -F_VOLSUM, -WEIGHT_W56_ATPONLY, -WEIGHT_W56, -F_ATTEND
  )

# Get variable names from original dataset
var_names <- dput(names(raw_data))
# Convert variable names to single column with 183 rows
var_names <- cbind(var_names)

# Get questions from questionnaire corresponding to each variable
questions <- c(
  "Marital Status",
  "Current Committed Relationship Status",
  "Have you ever been in a committed romantic relationship?",
  "Are you currently casually dating anyone?",
  "What they are seeking in their dating/romantic life",
  "Whether \"Just like being single\" is a reason why not seeking relationship/dating",
  "Whether \"Have more important priorities right now\" is a reason why not seeking relationship/dating",
  "Whether \"Feel like I am too old to date\" is a reason why not seeking relationship/dating",
  "Whether \"Have health problems that make it difficult to date\" is a reason why not seeking relationship/dating",
  "Whether \"Haven’t had luck with dating or relationships in the past\" is a reason why not seeking relationship/dating",
  "Whether \"Too busy\" is a reason why not seeking relationship/dating",
  "Whether \"Feel like no one would be interested in dating me\" is a reason why not seeking relationship/dating",
  "Whether \"Not ready to date after losing my spouse (if widowed) or ending a relationship\" is a reason why not seeking relationship/dating",
  "How long have you been in your current romantic relationship?",
  "Overall, would you say that things in your relationship are going...",
  "Overall, would you say that things in your dating life are going...",
  "Have you ever used an online dating site or dating app?",
  "Are you currently using an online dating site or dating app?",
  "How did you first meet your spouse or partner?",
  "Where online did you first meet your spouse or partner?",
  "Compared to 10 years ago, for most people, do you think dating is...",
  "Is giving a hug acceptable on a first date?",
  "Is kissing acceptable on a first date?",
  "Is having sex acceptable on a first date?",
  "Is sex between unmarried adults who are in a committed relationship acceptable?",
  "Is having an open relationship- that is, a committed relationship where both people agree that it is acceptable to date or have sex with other people acceptable?",
  "Is casual sex between consenting adults who are not in a committed relationship acceptable?",
  "Is two consenting adults exchanging sexually explicit images of themselves acceptable?",
  "Is kissing someone on a date without asking permission first acceptable?",
  "Have you ever searched for information online about someone you were romantically interested in?",
  "Regardless of whether you would do it yourself, do you think it’s ever acceptable for someone to look through their significant other’s cellphone without their knowledge?",
  "If you decided after a first date that you didn’t want to go out with that person again, what is the most likely way you would let them know?",
  "Is it acceptable to break up with someone you're casually dating in person?",
  "Is it acceptable to break up with someone you're casually dating through a phone call?",
  "Is it acceptable to break up with someone you're casually dating through email?",
  "Is it acceptable to break up with someone you're casually dating through a private message on a social media site?",
  "Is it acceptable to break up with someone you're casually dating through a text message?",
  "Is it acceptable to break up with someone you're in a committed relationship with in person?",
  "Is it acceptable to break up with someone you're in a committed relationship with through a phone call?",
  "Is it acceptable to break up with someone you're in a committed relationship with through email?",
  "Is it acceptable to break up with someone you're in a committed relationship with through a private message on a social media site?",
  "Is it acceptable to break up with someone you're in a committed relationship with through a text message?",
  "Do you think the increased focus on sexual harassment and assault over the last few years has made it easier or harder for MEN to know how to interact with someone they’re on a date with?",
  "Do you think the increased focus on sexual harassment and assault over the last few years has made it easier or harder for WOMEN to know how to interact with someone they’re on a date with?",
  "Overall, what type of effect would you say online dating sites and dating apps have had on dating and relationships?",
  "Compared to relationships that begin in person, in general, do you think relationships where people first meet through an online dating site or dating app are…",
  "In general, how safe do you think online dating sites and dating apps are as a way to meet people?",
  "How common is people being harassed or bullied on online dating sites and dating apps?",
  "How common is people receiving sexually explicit messages or images they did not ask for on online dating sites and dating apps?",
  "How common is people lying about themselves to appear more desirable on online dating sites and dating apps?",
  "How common are privacy violations, such as data breaches or identity theft on online dating sites and dating apps?",
  "How common is people setting up fake accounts in order to scam others on online dating sites and dating apps?",
  "Would you ever consider being in a committed relationship with someone who is of a different religion than you?",
  "Would you ever consider being in a committed relationship with someone who is of a different race or ethnicity than you?",
  "Would you ever consider being in a committed relationship with someone who has a significant amount of debt?",
  "Would you ever consider being in a committed relationship with someone who is raising children from another relationship?",
  "Would you ever consider being in a committed relationship with someone who lives far away from you?",
  "Would you ever consider being in a committed relationship with someone who is a Republican?",
  "Would you ever consider being in a committed relationship with someone who is a Democrat?",
  "Would you ever consider being in a committed relationship with someone who makes significantly more money than you?",
  "Would you ever consider being in a committed relationship with someone who makes significantly less money than you?",
  "Would you ever consider being in a committed relationship with someone who voted for Donald Trump?",
  "Would you ever consider being in a committed relationship with someone who voted for Hillary Clinton?",
  "Would you ever consider being in a committed relationship with someone who is 10 years older than you?",
  "Would you ever consider being in a committed relationship with someone who is 10 years younger than you?",
  "How much pressure, if any, do you feel from family members to be in a committed relationship?",
  "How much pressure, if any, do you feel from your friends to be in a committed relationship?",
  "How much pressure, if any, do you feel from society to be in a committed relationship?",
  "In the past year, how easy or difficult has it been for you to find people to date?",
  "It has been difficult for you to find people to date because... there is a limited number of people in my area for me to date",
  "It has been difficult for you to find people to date because... it's hard for me to find someone who meets my expectations",
  "It has been difficult for you to find people to date because... it's hard to find someone who's looking for the same type of relationship as me",
  "It has been difficult for you to find people to date because... it's hard for me to approach people",
  "It has been difficult for you to find people to date because... people aren't interested in dating me",
  "It has been difficult for you to find people to date because... I'm too busy",
  "Overall, would you say your OWN personal experiences with online dating sites or dating apps have been…",
  "In general in the past year, has using online dating sites or dating apps made you feel more confident or insecure?",
  "In general in the past year, has using online dating sites or dating apps made you feel more optimistic or pessimistic?",
  "In general in the past year, has using online dating sites or dating apps made you feel more hopeful or frustrated?",
  "Have you ever gone on a date with someone you met through an online dating site or dating app?",
  "Have you ever been in a committed relationship or married someone you first met through an online dating site or dating app?",
  "Have you ever come across the online dating profile of someone you already know offline?",
  "How important is it to you that online profiles included hobbies and interests?",
  "How important is it to you that online profiles included political affiliation?",
  "How important is it to you that online profiles included religious beliefs?",
  "How important is it to you that online profiles included occupation?",
  "How important is it to you that online profiles included racial or ethnic background?",
  "How important is it to you that online profiles included height?",
  "How important is it to you that online profiles included if they have children?",
  "How important is it to you that online profiles included type of relationship they're looking for?",
  "How important is it to you that online profiles included photos of themselves?",
  "How easy or difficult was it for you to find people on online dating sites or dating apps who you were physically attracted to?",
  "How easy or difficult was it for you to find people on online dating sites or dating apps who shared your hobbies and interests?",
  
  "How easy or difficult was it for you to find people on online dating sites or dating apps who were looking for the same kind of relationship as you?",
  "How easy or difficult was it for you to find people on online dating sites or dating apps who seemed like someone you would want to meet in person?",
  "How would you characterize the number of messages you have received on dating sites/apps?",
  "How would you characterize the number of messages you have received from people you were interested in on dating sites/apps?",
  "How well, if at all, do you feel you understand why online dating sites or dating apps present certain people as potential matches for you?",
  "How concerned are you, if at all, about how much data online dating sites or dating apps collect about you?",
  "Do you ever use social media sites, like Facebook, Twitter, or Instagram?",
  "How often, if ever, do you see people posting things about their romantic relationships on social media?",
  "In general, do the posts you see on social media about other people’s romantic relationships make you feel better or worse about your own relationships?",
  "In general, do the posts you see on social media about other people’s romantic relationships make you feel better or worse about your own dating life?",
  "Have you ever used social media to check up on someone that you used to date or be in a relationship with?",
  "Have you ever used social media to share or discuss things about your relationship or dating life?",
  "As far as you know, does your spouse or partner have a cellphone?",
  "As far as you know, does your spouse or partner use social media sites?",
  "As far as you know, does your spouse or partner play video games on a computer, game console or cellphone?",
  "How important, if at all, is social media to you personally when it comes to keeping up with what's going on your spouse's or partner's life?",
  "How important, if at all, is social media to you personally when it comes to showing how much you care about your spouse or partner?",
  "Have you ever felt jealous or unsure about your relationship because of the way your current spouse or partner interacts with other people on social media?",
  "How often, if ever, do you feel as if your spouse or partner is distracted by their cellphone when you are trying to have a conversation with them?",
  "How often, if ever, are you bothered by the amount of time your spouse or partner spends on their cellphone?",
  "How often, if ever, are you bothered by the amount of time your spouse or partner spends on social media sites?",
  "How often, if ever, are you bothered by the amount of time your spouse or partner spends playing video games?",
  "Have you ever given your spouse or partner the password or passcode to your email account?",
  "Have you ever given your spouse or partner the password or passcode to any of your social media accounts?",
  "Have you ever given your spouse or partner the password or passcode to your cellphone?",
  "Have you ever looked through your current spouse's or partner's cellphone without their knowledge?",
  "Have you ever heard of ghosting?",
  "Have you ever heard of breadcrumbing?",
  "Have you ever heard of phubbing?",
  "Have you ever heard of catfishing?",
  "Have you ever heard of friends with benefits?",
  "Have you ever had someone you’ve gone out with suddenly stop answering your phone calls or messages without explanation (sometimes called “ghosting”)?",
  "Has someone you were dating or on a date with ever pressured you for sex?",
  "Has someone you were dating or on a date with ever touched you in a way that made you feel uncomfortable?",
  "Has someone you were dating or on a date with ever sent you sexually explicit images that you didn't ask for?",
  "As far as you know, has someone you were dating or been on a date with ever spread rumors about your sexual history?",
  "As far as you know, has someone you were dating or been on a date with ever shared a sexually explicit image of you without your consent?",
  "As far as you know, has someone you were dating or been on a date with ever publically shared your contact information or address without your permission?",
  "Thinking about your own personal experiences, has someone ever called you an offensive name ON AN ONLINE DATING SITE OR DATING APP?",
  "Thinking about your own personal experiences, has someone ever threatened to physically harm you ON AN ONLINE DATING SITE OR DATING APP?",
  "Thinking about your own personal experiences, has someone ever sent you a sexually explicit message or image you didn’t ask for ON AN ONLINE DATING SITE OR DATING APP?",
  "Thinking about your own personal experiences, has someone ever continued to contact you after you said you were not interested ON AN ONLINE DATING SITE OR DATING APP?",
  "What sex is your spouse or partner?",
  "Sexual orientation",
  "Whether or not live in a metropolitan area",
  "Region of the US they reside in",
  "Type of region they reside in",
  "Age category",
  "Sex",
  "Education",
  "Education (expanded)",
  "Race/Ethnicity",
  "Place of birth",
  "Citizenship Status",
  "Marital Status",
  "Religion",
  "Whether born-again or evangelical Christian",
  "Political Party (Democrat/Republican/Independent)",
  "Political Party (Democrat/Republican dichotimized)",
  "Political Party (Dem/Lean Dem or Rep/Lean Rep dichotomized)",
  "Income",
  "Income (trichotomized)",
  "Voting registration status",
  "Voting registration status (trichotomized)",
  "Political ideology"
)

# Convert questions to single col
questions <- cbind(questions)

# Combine variables and questions into df
lookup_questions <- data.frame(var_names, questions)

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
  theme = shinytheme("flatly"),
  navbarPage(
    "CSC/SDS 235 Final Project: Michelle, Lauryn, Grace",
    tabPanel(
      "Interactive Dashboard",
      sidebarLayout(
        sidebarPanel(
          
          ## selected = idea and syntax from https://shiny.rstudio.com/reference/shiny/0.12.2/selectInput.html
          selectInput(inputId = "variable1", label = "Choose a first variable", selected = "Current Committed Relationship Status", lookup_questions$questions),
          
          ## code for this conditional panel is directly copied and pasted from
          ## the example at https://shiny.rstudio.com/reference/shiny/1.3.0/conditionalPanel.html -----------
          
          selectInput(inputId = "plotType", label = "Plot Type", c(Bar = "bar", Heatmap = "count"), selected = "bar"),
          # Only show this panel if the plot type is a two-way count
          conditionalPanel(
            condition = "input.plotType == 'count'",
            selectInput(inputId = "variable2", label = "Choose a second variable", selected = "Region of the US they reside in", lookup_questions$questions),
          ),
          
          textOutput("disclaimer_text")
        ),
        
        ### ---------------------------------------------------------------------------------------
        
        ## the conditional plot code is based on the conditional panel code above
        mainPanel(
          conditionalPanel(
            condition = "input.plotType == 'bar'",
            # plotlyOutput and renderPlotly
            # from https://stackoverflow.com/questions/57085342/renderplotly-does-not-work-despite-not-having-any-errors
            plotlyOutput("plotbar"),
            br(),
            textOutput("numparticipantsasked"),
            br(),
            textOutput("numparticipantsaskedexplan")
          ),
          conditionalPanel(
            condition = "input.plotType == 'count'",
            plotlyOutput("heatmap"),
            br(),
            textOutput("heatmaptextboxone"),
            textOutput("heatmaptextboxtwo"),
            br(),
            textOutput("countexplaintwo")
          )
        )
      )
    ),
    tabPanel(
      "Static Data Analysis",
      fluidRow(
        column(
          12,
          ## h3 from https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
          h3("About Our Project"),
          htmlOutput("aboutprojtext"),
          
          ## https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/ br idea
          br(),
          
          h3("Characterizing the Sample"),
          textOutput("characterizing_sample_text"),
          br(),
          fluidRow(
            column(1),
            column(5,
                   plotlyOutput("characterizingsamplemstatus"),
                   br(),
                   br()),
            column(5,
                   plotlyOutput("characterizingsampleorientation"),
                   br(),
                   br()
            ),
            column(1),
          ),
          fluidRow(
            column(1),
            column(5,
                   plotlyOutput("characterizingsampleideology"),
                   br(),
                   br()),
            column(5,
                   plotlyOutput("characterizingsamplerace"),
                   br(),
                   br()),
            column(1)
          ),
          fluidRow(
            column(1),
            column(5,
                   plotlyOutput("characterizingsampleage")),
            
            column(5,
                   plotlyOutput("characterizingsampleeduc")),
            column(1)
          )
          
        )),
      fluidRow(
        column(
          12,
          h3("Interesting Findings"),
          textOutput("overallinterestingfindingstext"),
          br(),
          fluidRow(column(3),
            column(6,
                   plotlyOutput("thingsindatinglife"),
                   br(),
                  textOutput("thingsdaatinglifetext"),
                  br(),
                  br()),
            column(3)
          ),
          fluidRow(
            column(6,
                   plotlyOutput("datinglifebyage"),
                   br()),
            column(6,
                   plotlyOutput("datinglifebysex"),
                   br())
          ),
          fluidRow(column(3),
            column(6,
            textOutput("summarydatinglife"),
            br(),
            br()
            ),
            column(3)
          ),
          
          fluidRow(column(3),
            column(6,
                 plotlyOutput("tenyears"),
                 br(),
                 textOutput("tenyearstext"),
                 br(),
                 br()),
            column(3)
          ),
          
          fluidRow(column(6,
                          plotlyOutput("tenyearsbyage"),
                          br()),
                   column(6,
                          plotlyOutput("tenyearsbysex"),
                          br())
                   ),
          
          fluidRow(column(3),
                   column(6,
                          textOutput("summarytenyears"),
                          br(),
                          br()
                   ),
                   column(3)
          ),
          
          fluidRow(column(3),
                   column(6,
                          plotlyOutput("feelings_by_sex"),
                          br(),
                          textOutput("onlinenegfeelingstext"),
                          br(),
                          br()
                   ),
                   column(3)
          ),
          
          fluidRow(column(3),
                   column(6,
                          plotlyOutput("bullingharass"),
                          br(),
                          textOutput("bullingharasstext"),
                          br(),
                          br()
                   ),
                   column(3)
          ),
          
          fluidRow(
            column(2),
            column(6, 
                   plotlyOutput("effectofonline"),
                   br(),
                   br()
            ),
            column(2,
                   textOutput("summaryonline"),
                   br(),
                   br()),
            column(2)
          )
        )
      ),
      
      
      
      fluidRow(
        column(
          12,
          ## footer hr() from https://stackoverflow.com/questions/30205034/shiny-layout-how-to-add-footer-disclaimer/38241035 
          hr(),
          
          h4("References"),
          htmlOutput("citations_textone"),
          htmlOutput("citations_texttwo"),
          br()
        )
      )
    )
  )
)




## code copied and modified from https://mastering-shiny.org/basic-app.html and
# https://mastering-shiny.org/basic-ui.html
server <- function(input, output, session) {
  output$plotbar <- renderPlotly({
    g <- ggplot((raw_data %>%
                   
                   ## remove NAs https://www.edureka.co/community/634/how-to-remove-na-values-with-dplyr-filter
                   filter(!is.na(get(lookup_questions %>%
                                       filter(questions == input$variable1) %>%
                                       pull(var_names[1])))) %>%
                   group_by_(
                     lookup_questions %>%
                       filter(questions == input$variable1) %>%
                       pull(var_names[1])) %>%
                   summarize(
                     n = n(),
                   ) %>%
                   mutate(
                     pct = n/sum(n)
                   )), aes(
                     x = get(lookup_questions %>%
                               filter(questions == input$variable1) %>%
                               pull(var_names[1])),
                     y = n,
                     text = paste(paste("Number of Participants:", n, sep = " "), paste("Percentage:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "), sep = "<br>")
                   )) +
      geom_col() +
      
      xlab(str_wrap(input$variable1)) +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            #Attempt to fix margin - does not work
            #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))
            #This does not work either:
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    
    ggplotly(g, tooltip = "text")
  })
  
  output$numparticipantsasked <- renderText({
    total_asked <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable1) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable1) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(num_not_na[1])
    
    total_people <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable1) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable1) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(n[1])
    
    text <- paste("Note:", 
                  paste(
                    paste(total_asked, total_people, sep = "/"), 
                    "participants were asked this question."), sep = " ")
  })
  
  
  output$numparticipantsaskedexplan <- renderText(
    "Whether or not participants
    were asked certain questions was often conditional on previous 
    responses. For example, only those who are married were not asked
    whether they were in a committed relationship."
  )
  
  output$countexplaintwo <- renderText(
    "Whether or not participants
    were asked certain questions was often conditional on previous 
    responses. For example, only those who are married were not asked
    whether they were in a committed relationship."
  )
  
  # Attempt to build heatmap
  
  output$heatmap <- renderPlotly({
    if(input$variable1 != input$variable2){
      
      g <- raw_data %>%
        filter(!is.na(get(lookup_questions %>%
                            filter(questions == input$variable1) %>%
                            pull(var_names[1])))) %>%
        filter(!is.na(get(lookup_questions %>%
                            filter(questions == input$variable2) %>%
                            pull(var_names[1])))) %>%
        ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
        group_by_(
          lookup_questions %>%
            filter(questions == input$variable1) %>%
            pull(var_names[1]),
          lookup_questions %>%
            filter(questions == input$variable2) %>%
            pull(var_names[1])
        ) %>%
        summarize(
          n = n()
        ) %>%
        ggplot(aes(
          x = get(lookup_questions %>%
                    filter(questions == input$variable1) %>%
                    pull(var_names[1])),
          y = get(lookup_questions %>%
                    filter(questions == input$variable2) %>%
                    pull(var_names[1])),
          fill = n
        )) +
        geom_tile() +
        
        ## HTML color codes from https://htmlcolorcodes.com/
        ## scale fill gradient idea and syntax from https://ggplot2.tidyverse.org/reference/scale_gradient.html
        scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") +
        xlab(str_wrap(input$variable1)) +
        ylab(str_wrap(input$variable2)) +
        ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
      
      ## tooltip from 
      ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
      ## and
      ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
      ggplotly(g, tooltip = "fill")
    }
    else{
      g <- raw_data %>%
        ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
        group_by_(
          lookup_questions %>%
            filter(questions == input$variable1) %>%
            pull(var_names[1])) %>%
        summarize(
          n = n()
        ) %>%
        ggplot(aes(
          x = get(lookup_questions %>%
                    filter(questions == input$variable1) %>%
                    pull(var_names[1])),
          y = n
        )) +
        geom_col() +
        
        xlab(str_wrap(input$variable1)) +
        ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
      
      ## tooltip from 
      ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
      ## and
      ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
      ggplotly(g, tooltip = "y")
    }
  })
  
  output$heatmaptextboxone <- renderText({
    total_asked <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable1) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable1) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(num_not_na[1])
    
    total_people <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable1) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable1) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(n[1])
    
    text <- paste(paste("Note:", 
                        paste(
                          paste(total_asked, total_people, sep = "/"), 
                          "participants were asked"), sep = " "), str_wrap(input$variable1))
  })
  
  
  output$heatmaptextboxtwo <- renderText({
    total_asked <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable2) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable2) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(num_not_na[1])
    
    total_people <- raw_data %>%
      select(
        lookup_questions %>%
          filter(questions == input$variable2) %>%
          pull(var_names[1])) %>%
      summarize(
        n = n(),
        
        ## count NAs https://stackoverflow.com/questions/44290704/count-non-na-values-by-group
        num_not_na = sum(!is.na(get(lookup_questions %>%
                                      filter(questions == input$variable2) %>%
                                      pull(var_names[1]))))
      ) %>%
      pull(n[1])
    
    text <- paste(paste("Note:", 
                        paste(
                          paste(total_asked, total_people, sep = "/"), 
                          "participants were asked"), sep = " "), str_wrap(input$variable2))
  })
  
  
  output$disclaimer_text <- renderText(
    "Disclaimer: Some question text was changed for clarity or conciseness"
  )
  
  output$citations_textone <- renderUI(HTML(
    "Vogels, E. A., & Anderson, M. (2020, May 8). 
    Dating and Relationships in the Digital Age. Pew Research Center. <a href ='https://www.pewresearch.org/internet/2020/05/08/dating-and-relationships-in-the-digital-age/'>Link to the data</a>."
  ))
  
  output$citations_texttwo <- renderUI(HTML(
    "Pew Research Center. (2019). 
    Pew Research Center’s American Trends Panel Wave 56 Methodology Report. 
    Downloaded as metadata alongside the data from <a href ='https://www.pewresearch.org/internet/2020/05/08/dating-and-relationships-in-the-digital-age/'>this link</a>"
  ))
  
  #Static plots for Interesting Findings
  output$feelings_by_sex <- renderPlotly({
    
    
    raw_data$ONFEEL.c_W56 <- factor(raw_data$ONFEEL.c_W56,levels = c("Frustrated", "Neither", "Hopeful", "Refused"))
    
    g <- raw_data %>%
      filter(!is.na(F_SEX) & !is.na(ONFEEL.c_W56)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by_(
        lookup_questions %>%
          filter(questions == "In general in the past year, has using online dating sites or dating apps made you feel more hopeful or frustrated?") %>%
          pull(var_names[1]),
        lookup_questions %>%
          filter(questions == "Sex") %>%
          pull(var_names[1])
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(x = ONFEEL.c_W56, y =F_SEX, fill = n)) +
      geom_tile() +
      ggtitle("Feelings After Using Online Dating, Sorted by Sex") +
      xlab(str_wrap("In general in the past year, has using online dating sites or dating apps made you feel more hopeful or frustrated?"))+
      ylab("Sex")+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") 
    
    ggplotly(g, tooltip = "fill")
  })
  
  output$jealousy_by_sex <- renderPlot(
    raw_data %>%
      filter(!is.na(F_SEX) & !is.na(SNSFEEL_W56)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by_(
        lookup_questions %>%
          filter(questions == "Have you ever felt jealous or unsure about your relationship because of the way your current spouse or partner interacts with other people on social media?") %>%
          pull(var_names[1]),
        lookup_questions %>%
          filter(questions == "Sex") %>%
          pull(var_names[1])
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(x = F_SEX, y = SNSFEEL_W56, fill = n)) +
      geom_tile() +
      ggtitle("Feelings Based on Partner's Social Media Use, Sorted by Sex") +
      xlab("Sex")+
      ylab("Have you ever felt jealous or unsure about your relationship because of the way your
           current spouse or partner interacts with other people on social media?")+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") 
    
  )
  
  
  output$aboutprojtext<- renderUI(HTML("This application provides an analysis of and means to 
        interact with data from the 2019 Pew Research Center survey on the 
                           intersection between romantic relationships and technology. The set of participants recruited for the survey, part of the American Trends Panel, were designed to serve as a representative sample of the US (Pew Research Center, 2019). 
                           Download the dataset with a Pew Research Center account and view their 
                           analysis <a href ='https://www.pewresearch.org/internet/2020/05/08/dating-and-relationships-in-the-digital-age/'>here</a> (Vogels & Anderson, 2020)."))
  output$onlinenegfeelingstext <- renderText("Women tend to experience more negative feelings regarding relationships and social media. For people who used online dating, more women felt pessimistic (41% of all women asked this question) than men (35%).")
  
  
  output$textbtwo <- renderText("Additionally, more women in committed relationships reported feeling insecure because of their partner's social media use (30%) than men (15%).")
  output$characterizing_sample_text <- renderText("This sample is largely married (40%), straight (68%), politically moderate (35%) or liberal (26%), non-Hispanic white (68%), and ages 30-64 (64%) with a college degree or higher (46%).")
  
  output$characterizingsamplemstatus <- renderPlotly({
    g <- raw_data %>%
      group_by(MARITAL_W56) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      # reorder from https://sebastiansauer.github.io/ordering-bars/
      ggplot(aes(x = reorder(MARITAL_W56, -n), y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Marital Status") +
      ylab("Number of Participants") +
      ggtitle("Number of Participants by Marital Status") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip = "text" with text specified above idea from https://plotly.com/ggplot2/interactive-tooltip/
    ggplotly(g, tooltip = 'text')
  })
  
  output$characterizingsampleorientation <- renderPlotly({
    g <- raw_data %>%
      group_by(ORIENTATIONMOD_W56) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      # reorder from https://sebastiansauer.github.io/ordering-bars/
      ggplot(aes(x = reorder(ORIENTATIONMOD_W56, -n), y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Sexual Orientation") +
      ggtitle("Number of Participants by Sexual Orientation") +
      ylab("Number of Participants") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ggplotly(g, tooltip = 'text')
  })
  
  output$characterizingsampleideology <- renderPlotly({
    ## reorder from https://sebastiansauer.github.io/ordering-bars/
    raw_data$F_IDEO <- factor(raw_data$F_IDEO,levels = c("Very conservative", "Conservative", "Moderate", "Liberal", "Very liberal", "Refused"))
    
    g <- raw_data %>%
      group_by(F_IDEO) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      ggplot(aes(x = F_IDEO, y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Political Ideology") +
      ggtitle("Number of Participants by Political Ideology") +
      ylab("Number of Participants") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ggplotly(g, tooltip = 'text')
  })
  
  output$characterizingsamplerace <- renderPlotly({
    g <- raw_data %>%
      group_by(F_RACETHN) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      ggplot(aes(x = F_RACETHN, y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Race/Ethnicity") +
      ggtitle("Number of Participants by Race/Ethnicity") +
      ylab("Number of Participants") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ggplotly(g, tooltip = 'text')
  })
  
  output$characterizingsampleage <- renderPlotly({
    g <- raw_data %>%
      group_by(F_AGECAT) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      ggplot(aes(x = F_AGECAT, y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Age") +
      ggtitle("Number of Participants by Age") +
      ylab("Number of Participants") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ggplotly(g, tooltip = 'text')
  })
  
  output$characterizingsampleeduc <- renderPlotly({
    ## reorder from https://sebastiansauer.github.io/ordering-bars/
    raw_data$F_EDUCCAT <- factor(raw_data$F_EDUCCAT,levels = c("H.S. graduate or less", "Some College", "College graduate+", "Don't know/Refused"))
    
    
    g <- raw_data %>%
      group_by(F_EDUCCAT) %>%
      summarize(
        n = n()
      ) %>% 
      mutate(
        pct = n/sum(n)
      ) %>%
      ggplot(aes(x = F_EDUCCAT, y = n, text = paste("Percent of Total:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "))) +
      geom_col() +
      xlab("Education Level") +
      ggtitle("Number of Participants by Education") +
      ylab("Number of Participants") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(vjust = 1))
    
    ggplotly(g, tooltip = 'text')
  })
  
  output$thingsindatinglife <- renderPlotly({
    raw_data$FAMSURV19DATING_W56 <- factor(raw_data$FAMSURV19DATING_W56,levels = c("Not at all well", "Not too well", "Fairly well", "Very well", "Refused"))
    
    g <- ggplot((raw_data %>%
                   
                   ## remove NAs https://www.edureka.co/community/634/how-to-remove-na-values-with-dplyr-filter
                   filter(!is.na(FAMSURV19DATING_W56)) %>%
                   group_by(FAMSURV19DATING_W56) %>%
                   summarize(
                     n = n(),
                   ) %>%
                   mutate(
                     pct = n/sum(n)
                   )), aes(
                     x = FAMSURV19DATING_W56,
                     y = n,
                     text = paste(paste("Number of Participants:", n, sep = " "), paste("Percentage:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "), sep = "<br>")
                   )) +
      geom_col() +
      ggtitle("How Participants' Dating Lives are Going") +
      xlab("Overall, would you say that things in your dating life are going...") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous("Number of Participants", expand = c(0,0)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            #Attempt to fix margin - does not work
            #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))
            #This does not work either:
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    
    ggplotly(g, tooltip = "text")
  })
  
  output$thingsdaatinglifetext <- renderText("The majority of participants (69%) said that things in their dating
                                             life are going not at all well or not too well. This highlights the trouble
                                             many are facing with modern dating, whether participants' problems are technology related or not.")
 
  
  output$tenyears <- renderPlotly({
    raw_data$DATE10YR_W56 <- factor(raw_data$DATE10YR_W56,levels = c("Harder today", "About the same", "Easier today", "Refused"))
    
    g <- ggplot((raw_data %>%
                   
                   ## remove NAs https://www.edureka.co/community/634/how-to-remove-na-values-with-dplyr-filter
                   filter(!is.na(DATE10YR_W56)) %>%
                   group_by(DATE10YR_W56) %>%
                   summarize(
                     n = n(),
                   ) %>%
                   mutate(
                     pct = n/sum(n)
                   )), aes(
                     x = DATE10YR_W56,
                     y = n,
                     text = paste(paste("Number of Participants:", n, sep = " "), paste("Percentage:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "), sep = "<br>")
                   )) +
      geom_col() +
      xlab("Compared to 10 years ago, for most people, do you think dating is...") +
      ggtitle("Difficulty of Dating Now Compared to the Past") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous("Number of Participants", expand = c(0,0)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            #Attempt to fix margin - does not work
            #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))
            #This does not work either:
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    
    ggplotly(g, tooltip = "text")
    
  })
  
  output$tenyearstext <- renderText("A plurality of respondents said that dating is harder today than it was 10 years
                                    ago (48%), while only 18% think that dating is easier today.")
 
  
  output$overallinterestingfindingstext <- renderText("Many, but not all, participants expressed struggles or dissatisfaction
                                                      with modern dating. On this survey, which was collected before the COVID-19 pandemic,
                                                      participants identifying as male and female, and across ages, reported difficulties.")
  
  output$datinglifebysex <- renderPlotly({
    raw_data$FAMSURV19DATING_W56 <- factor(raw_data$FAMSURV19DATING_W56,levels = c("Not at all well", "Not too well", "Fairly well", "Very well", "Refused"))
    
    g <- raw_data %>%
      filter(!is.na(FAMSURV19DATING_W56)) %>%
      filter(!is.na(F_SEX)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by(FAMSURV19DATING_W56,
               F_SEX
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(
        x = FAMSURV19DATING_W56,
        y = F_SEX,
        fill = n
      )) +
      geom_tile() +
      ggtitle("How Participants' Dating Lives are Going, Sorted by Sex") +
      
      ## HTML color codes from https://htmlcolorcodes.com/
      ## scale fill gradient idea and syntax from https://ggplot2.tidyverse.org/reference/scale_gradient.html
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") +
      xlab("Overall, would you say that things in your dating life are going...") +
      ylab("Sex") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    ggplotly(g, tooltip = "fill")
  })
  
  output$datinglifebyage <- renderPlotly({
    raw_data$FAMSURV19DATING_W56 <- factor(raw_data$FAMSURV19DATING_W56,levels = c("Not at all well", "Not too well", "Fairly well", "Very well", "Refused"))
    
    g <- raw_data %>%
      filter(!is.na(FAMSURV19DATING_W56)) %>%
      filter(!is.na(F_AGECAT)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by(FAMSURV19DATING_W56,
               F_AGECAT
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(
        x = FAMSURV19DATING_W56,
        y = F_AGECAT,
        fill = n
      )) +
      geom_tile() +
      ggtitle("How Participants' Dating Lives are Going, Sorted by Age") +
      
      ## HTML color codes from https://htmlcolorcodes.com/
      ## scale fill gradient idea and syntax from https://ggplot2.tidyverse.org/reference/scale_gradient.html
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") +
      xlab("Overall, would you say that things in your dating life are going...") +
      ylab("Age") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    ggplotly(g, tooltip = "fill")
  })
  
  
  output$summarydatinglife <- renderText("Findings about most people having some trouble with their dating life
                                         are found across ages and sexes.")
  
  
  output$tenyearsbysex <- renderPlotly({
    raw_data$DATE10YR_W56 <- factor(raw_data$DATE10YR_W56,levels = c("Harder today", "About the same", "Easier today", "Refused"))
    
    g <- raw_data %>%
      filter(!is.na(DATE10YR_W56)) %>%
      filter(!is.na(F_SEX)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by(DATE10YR_W56,
               F_SEX
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(
        x = DATE10YR_W56,
        y = F_SEX,
        fill = n
      )) +
      geom_tile() +
      ggtitle("Difficulty of Dating Now Compared to the Past, Sorted by Sex") +
      
      ## HTML color codes from https://htmlcolorcodes.com/
      ## scale fill gradient idea and syntax from https://ggplot2.tidyverse.org/reference/scale_gradient.html
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") +
      xlab("Compared to 10 years ago, for most people, do you think dating is...") +
      ylab("Sex") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    ggplotly(g, tooltip = "fill")
  })
  
  output$tenyearsbyage <- renderPlotly({
    raw_data$DATE10YR_W56 <- factor(raw_data$DATE10YR_W56,levels = c("Harder today", "About the same", "Easier today", "Refused"))
    
    g <- raw_data %>%
      filter(!is.na(DATE10YR_W56)) %>%
      filter(!is.na(F_AGECAT)) %>%
      ### group_by_ from https://stackoverflow.com/questions/54482025/call-input-in-shiny-for-a-group-by-function
      group_by(DATE10YR_W56,
               F_AGECAT
      ) %>%
      summarize(
        n = n()
      ) %>%
      ggplot(aes(
        x = DATE10YR_W56,
        y = F_AGECAT,
        fill = n
      )) +
      geom_tile() +
      ggtitle("Difficulty of Dating Now Compared to the Past, Sorted by Age") +
      
      ## HTML color codes from https://htmlcolorcodes.com/
      ## scale fill gradient idea and syntax from https://ggplot2.tidyverse.org/reference/scale_gradient.html
      scale_fill_gradient(low = "#FFFFFF", high = "#000773", na.value = "#8E8E8E") +
      xlab("Compared to 10 years ago, for most people, do you think dating is...") +
      ylab("Age") +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    ggplotly(g, tooltip = "fill")
  })
  
  output$summarytenyears <- renderText("Those who identify as female seem to be more pessimistic about current dating conditions than those identifying as
                                       male, and younger people (under the age of 49) seem to be somewhat more pessimistic than those
                                       over the age of 50.")
  
  output$effectofonline <- renderPlotly({
    raw_data$ONIMPACT_W56 <- factor(raw_data$ONIMPACT_W56,levels = c("Mostly negative effect", "Neither positive or negative effect", "Mostly positive effect", "Refused"))
    
    g <- ggplot((raw_data %>%
                   
                   ## remove NAs https://www.edureka.co/community/634/how-to-remove-na-values-with-dplyr-filter
                   filter(!is.na(ONIMPACT_W56)) %>%
                   group_by(ONIMPACT_W56) %>%
                   summarize(
                     n = n(),
                   ) %>%
                   mutate(
                     pct = n/sum(n)
                   )), aes(
                     x = ONIMPACT_W56,
                     y = n,
                     text = paste(paste("Number of Participants:", n, sep = " "), paste("Percentage:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "), sep = "<br>")
                   )) +
      geom_col() +
      ggtitle("Perceived Effect of Online Datinig") +
      xlab(str_wrap("Overall, what type of effect would you say online dating sites and dating apps have had on dating and relationships?")) +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous("Number of Participants", expand = c(0,0)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            #Attempt to fix margin - does not work
            #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))
            #This does not work either:
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    
    ggplotly(g, tooltip = "text")
  })
  
  output$summaryonline <- renderText("Despite these findings about pesimission with regard to
                                     recent dating in general, people's current dating lives, and the downsides of online dating,
                                     the participants were mixed on whether online dating has improved
                                     or worsened online dating in general. A plurality of participants (48%)
                                     said that online dating had neither effect. This brings up the question,
                                     if online dating is not what is making modern dating more difficult, then what is?")
  
  
  output$bullingharass <- renderPlotly({
    raw_data$ONPROBLEM.a_W56 <- factor(raw_data$ONPROBLEM.a_W56,levels = c("Not at all common", "Not too common", "Somewhat common", "Very common", "Refused"))
    
    g <- ggplot((raw_data %>%
                   
                   ## remove NAs https://www.edureka.co/community/634/how-to-remove-na-values-with-dplyr-filter
                   filter(!is.na(ONPROBLEM.a_W56)) %>%
                   group_by(ONPROBLEM.a_W56) %>%
                   summarize(
                     n = n(),
                   ) %>%
                   mutate(
                     pct = n/sum(n)
                   )), aes(
                     x = ONPROBLEM.a_W56,
                     y = n,
                     text = paste(paste("Number of Participants:", n, sep = " "), paste("Percentage:", paste(100*round(pct, digits = 2), "%", sep = ""), sep = " "), sep = "<br>")
                   )) +
      geom_col() +
      ggtitle("Perceived Prevalence of Bullying/Harassment in Online Dating") +
      xlab(str_wrap("How common is people being harassed or bullied on online dating sites and dating apps?")) +
      ## Wrapping axis ticks https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous("Number of Participants", expand = c(0,0)) +
      theme(panel.background = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_line(color = "black"),
            #Attempt to fix margin - does not work
            #axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))
            #This does not work either:
            axis.title.x = element_text(vjust = 1))
    
    ## tooltip from 
    ## https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524
    ## and
    ## https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly
    
    ggplotly(g, tooltip = "text")
  })
  
  output$bullingharasstext <- renderText("Bullying and harassment appears to be a problem on online dating apps and 
                                         websites. 59% of participants said that this was somewhat or very common, 
                                         a concerning statistic.")
}

# Run the application
shinyApp(ui = ui, server = server)