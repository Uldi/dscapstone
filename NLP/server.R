#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(futile.logger)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    flog.info("load NLP prediction model")
    setwd("/Users/david/Coursera/assignments/dscapstone")
    flog.trace(getwd())
    #ngramSBOTables <- setupTestNLP()
    #ngramSBOTables <- loadSBOModel("sample")
    ngramSBOTables <- loadSBOModel("full25")
    text <- ""
    flog.info("load NLP prediction model - completed")
    
    predictedText <- reactive({
        textInput <- input$textInput
        predictSBONLP(ngramSBOTables, textInput)
    })
    
    
    output$textDebug<- renderPrint({ predictedText() })
    output$textOutput <- renderText({predictedText()})
    
    observe({
        textInput = input$dynText
        #hier habe ich noch ein loop problem...
        if(grepl("  $", textInput)) {
            updateTextInput(session,"dynText",value=predictSBONLP(ngramSBOTables, textInput))
        } else if (grepl("[\\.!?;,:]$", textInput)) {
            text <<- paste(text, textInput, sep=" ")
            updateTextInput(session, "dynText", value="")
            output$fixedText <- renderText({text})
        }
    })
})

