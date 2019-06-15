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
# library(pryr)
source('prediction.R')
# source('loaddata.R')
# source('corpusFunctions.R')
# source('modelBuilding.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    flog.info("load NLP prediction model")
    flog.threshold(INFO)
    # setwd("/Users/david/Coursera/assignments/dscapstone/NLP")
    flog.info("working dir=%s",getwd())
    flog.trace("load 1")
    kbo1 <- readRDS("data/model/modelKBO1.rds")
    flog.trace("load 2")
    kbo2 <- readRDS("data/model/modelKBO2.rds")
    flog.trace("load 3")
    kbo3 <- readRDS("data/model/modelKBO3.rds")
    flog.trace("load 4")
    kbo4 <- readRDS("data/model/modelKBO4.rds")
    kbo <- list(kbo1, kbo2, kbo3, kbo4)
    
    gtTables <<- readGTTables()
    gtTablesK <<- 5
    
    filterStopwords = FALSE
    predStopwords <<- readRDS("data/model/stopwords.rds")
    predFilterStopwords <<- filterStopwords
    
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

