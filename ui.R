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
  tabPanel("Traffic flow ranking", fluidPage(
    # Input
    fluidRow(
      style = "margin-top:50px;margin-bottom:20px",
      
      column(
        width = 2,
        offset = 4,
        uiOutput("timeslots")
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
    # end Input
    
    
    # Main
    fluidRow(
      # Map
      column(
        width = 6,
        leafletOutput("map", height = 550)
      ),
      # end Map
      
      # Result
      column(
        width = 6,
        tabsetPanel(
          tabPanel("View result as table", dataTableOutput("list"), style = "margin-top:20px"),
          tabPanel("View result as chart", plotlyOutput("chart"), style = "margin-top:20px")
        )
      )
      # end Result
    ))),
  tabPanel("Taxi behavior", fluidPage(
    # Input
    fluidRow(
      style = "margin-top:50px;margin-bottom:20px",
      
      column(
        width = 3,
        offset = 3,
        uiOutput("users")
      ),
      
      column(
        width = 2,
        uiOutput("timeslots2")
      ),
      
      column(
        width = 4,
        style = "margin-top:20px",
        actionButton("submit2", "Submit", class = "btn btn-primary")
      )
    ),
    # end Input
    
    # Main
    fluidRow(
        leafletOutput("map2", height = 600)
    )))
))
