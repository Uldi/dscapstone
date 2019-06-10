#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Predict Next Word"),
        h4("DS Capstone Project, David Ulrich, June 2019"),
        hr(),
        h4("Enter your text - get next word predicted"), 
        h5("Instruction:"),
        tags$ul(
            tags$li("for inline word prediction just enter 2 spaces or click on predict"),
            tags$li("type a punctuation to start a new sentence"),
            tags$li("give shinyapps.io a few seconds to load the model")
        ),
        br(),
        fluidRow(
            column(10, textInput("dynText", label=NULL, value="", width="100%")),
            column(2, actionButton("predict", label="predict"))
            ),
        br(),
        fluidRow(column(10, textOutput("fixedText"))),
        hr()
    )
))
