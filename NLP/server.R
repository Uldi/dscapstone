#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(futile.logger)
library(dplyr)
library(quanteda)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    flog.info("load NLP prediction model")
    setwd("/Users/david/Coursera/assignments/dscapstone")
    flog.trace(getwd())
    # ngramKBOTables <- loadSBOModel("fullNoPW1F")
    # ngramKBOTables <- readKBOModel("fullNoPWNoSW1F")
    # ngramKBOTables <- readKBOModel("fullNoPW1FC")
    ngramKBOTables <- kboFull
    text <- ""
    flog.info("load NLP prediction model - completed")
    
    # predictedText <- reactive({
    #     textInput <- input$textInput
    #     predictNextWord(ngramKBOTables, textInput)
    # })
    
    predTable <- reactive({
        textInput <- input$dynText
        predTable <- getNextWordPredictionTable(ngramKBOTables, textInput)
    })
    
    
    output$textDebug<- renderPrint({ predictedText() })
    output$textOutput <- renderText({predictedText()})
    
    output$predictionTable <- DT::renderDataTable(
        DT::datatable(isolate({predTable()}), options = list(pageLength = 15))
    )
    
    proxy <- dataTableProxy('predictionTable', session)
    observe({
        textInput = input$dynText
        kValue = input$kValue
        alpha = input$alpha
        #hier habe ich noch ein loop problem...
        if(grepl("  $", textInput)) {
            updateTextInput(session,"dynText",value=predictNextWord(ngramKBOTables, textInput))
            replaceData(proxy, getNextWordPredictionTable(ngramKBOTables, textInput, kValue, alpha))
        } else if (grepl("[\\.!?;,:]$", textInput)) {
            text <<- paste(text, textInput, sep=" ")
            updateTextInput(session, "dynText", value="")
            output$fixedText <- renderText({text})
        }
    })
})

