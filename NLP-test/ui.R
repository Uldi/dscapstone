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
        titlePanel("NLP Simple Test"),
        hr(),
        h4("Enter your dynamic Input"), 
        h5("(for word proposal enter 2 spaces!)"),
        
        fluidRow(column(12, textInput("dynText", label=NULL, value="", width="100%"))),
        fluidRow(column(12, textOutput("fixedText"))),
        hr(),
        sliderInput("kValue", "k:",
                    min = 0, max = 100,
                    value = 5),
        
        # Input: Decimal interval with step value ----
        sliderInput("alpha", "alpha:",
                    min = 0, max = 1,
                    value = 0.2, step = 0.1),
        checkboxInput("kboCheckbox", label = "Use KBO instead of SBO", value = TRUE),
        
        DT::dataTableOutput('predictionTable')
    
#        ,
            # Copy the line below to make a text input box
#        wellPanel(
            #submitButton("Submit"),
#            textInput("textInput", label = h3("Text input"), value = ""),
          
#            hr(),
#            fluidRow(column(3, verbatimTextOutput("textDebug")))
#            fluidRow(column(3, verbatimTextOutput("textOutput"))),
#      )

    )
))
