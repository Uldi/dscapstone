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
  
  # Application title
  titlePanel("NLP Simple Test"),
  
  fluidPage(
      
      # Copy the line below to make a text input box
      textInput("textInput", label = h3("Text input"), value = "Enter text..."),
      submitButton("Submit"),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("textOutput")))
      
      
      
  )
))
