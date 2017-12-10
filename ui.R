#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Hanoi taxicab datamining", 
  theme = shinytheme("superhero"), position = "fixed-top",
  
  tabPanel("Traffic Flow Ranking", fluidPage(
    # Input
    fluidRow(
      style = "margin-top:50px;margin-bottom:20px",
      
      column(
        width = 2,
        offset = 2,
        uiOutput("timeslots")
      ),
      
      column(
        width = 2,
        uiOutput("days")
      ),
      
      column(
        width = 2,
        uiOutput("algorithms")
      ),
      
      column(
        width = 4,
        style = "margin-top:20px",
        actionButton("submit", "Submit", class = "btn btn-primary")
      )
    ),
    
    # Main
    fluidRow(
      # Map
      column(
        width = 6,
        leafletOutput("map", height = 550)
      ),
      
      # Table
      column(
        width = 6,
        dataTableOutput("table")
      )
    )
  )),
  
  tabPanel("Taxi Behavior", fluidPage(
    # Input
    fluidRow(
      style = "margin-top:50px;margin-bottom:20px",
      
      column(
        width = 7,
        offset = 5,
        fileInput("file", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      )
    ),
    
    # Main
    fluidRow(
      # Map
      column(
        width = 12,
        leafletOutput("map2", height = 600)
      )
    )
  ))
))
