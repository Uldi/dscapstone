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
source('prediction.R')
source('loaddata.R')
source('corpusFunctions.R')
source('modelBuilding.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    flog.info("load NLP prediction model")
    flog.threshold(TRACE)
    # setwd("/Users/david/Coursera/assignments/dscapstone")
    flog.trace(getwd())
    ngramKBOTables <- readKBOModel(modelName="model") 
    gtTables <<- readGTTables()
    gtTablesK <<- 5
    filterStopwords = FALSE
    text <- ""
    flog.info("load NLP prediction model - completed")
    
    # predictedText <- reactive({
    #     textInput <- input$textInput
    #     predictNextWord(ngramKBOTables, textInput)
    # })
    
    predTable <- reactive({
        textInput <- input$dynText
        predTable <- getNextWordPredictionTable(ngramKBOTables, textInput, filterStopwords=filterStopwords)
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
        useKBO = input$kboCheckbox
        #hier habe ich noch ein loop problem...
        if(grepl("  $", textInput)) {
            updateTextInput(session,"dynText",value=predictNextWord(ngramKBOTables, textInput, filterStopwords=filterStopwords))
            replaceData(proxy, getNextWordPredictionTable(ngramKBOTables, textInput, kValue, alpha, useKBO, filterStopwords=filterStopwords))
        } else if (grepl("[\\.!?;,:]$", textInput)) {
            text <<- paste(text, textInput, sep=" ")
            updateTextInput(session, "dynText", value="")
            output$fixedText <- renderText({text})
        }
    })
})

