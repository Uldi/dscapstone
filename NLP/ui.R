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
        titlePanel("DS Capstone Project"),
        h4("David Ulrich"),
        hr(),
        h4("Enter your text - get next word predicted"), 
        h5("Instruction:"),
        tags$ul(
            tags$li("for inline word prediction just enter 2 spaces"),
            tags$li("type a punctuation to start a new sentence"),
            tags$li("give shinyapps.io a few seconds to load the model")
        ),
        
        fluidRow(column(12, textInput("dynText", label=NULL, value="", width="100%"))),
        fluidRow(column(12, textOutput("fixedText"))),
        hr()
    )
))
