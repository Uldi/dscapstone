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
library(quanteda)
library(pryr)
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
    flog.trace("Mem Used: %f:", mem_used())
    
    kbo1 <- readRDS("data/model/modelKBO1.rds")
    kbo2 <- readRDS("data/model/modelKBO2.rds")
    kbo3 <- readRDS("data/model/modelKBO3.rds")
    kbo4 <- readRDS("data/model/modelKBO4.rds")
    kbo <- list(kbo1, kbo2, kbo3, kbo4)
    
    # ngramKBOTables <- readKBOModel(modelName="model") 
    flog.trace("model size %f=", object.size(kbo))
    flog.trace("Mem Used: %f:", mem_used())
    gtTables <<- readGTTables()
    gtTablesK <<- 5
    filterStopwords = FALSE
    text <- ""
    flog.info("load NLP prediction model - completed")
    

# UI handling... 
    
    observeEvent(input$predict, {
        flog.trace("observeEvent")
        textInput = input$dynText
        updateTextInput(session,"dynText",value=predictNextWord(kbo, textInput, filterStopwords=filterStopwords))
    })
    
    observe({
        textInput = input$dynText
        flog.trace("observe")
        if(grepl("  $", textInput)) {
            # flog.trace("observe: %s", textInput)
            updateTextInput(session,"dynText",value=predictNextWord(kbo, textInput, filterStopwords=filterStopwords))
        } else if (grepl("[\\.!?;,:]$", textInput)) {
            text <<- paste(text, textInput, sep=" ")
            updateTextInput(session, "dynText", value="")
            output$fixedText <- renderText({text})
        }
    })
})

