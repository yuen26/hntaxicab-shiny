#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(plotly)
library(data.table)
library(dplyr)

source("common/timeslot.R")
source("common/algorithm.R")
source("common/user.R")
timeslots <- getTimeslots()
algorithms <- getAlgorithms()
users <- getUsers()

source("ranking/input.R")
source("ranking/main.R")

shinyServer(function(input, output) {
  
  # ==============================================================
  # =================== TRAFFIC FLAW RANKING =====================
  # ==============================================================
  
  # ============================= MAP ============================
  source("ranking/region.R")
  regions <- drawMap()
  viewPoint <- getViewPoint(regions)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      setView(lng = viewPoint$lng, lat = viewPoint$lat, zoom = 13) %>%
      addTiles() %>%
      addRectangles(
        data = regions,
        lng1 = regions$east, lat1 = regions$north,
        lng2 = regions$west, lat2 = regions$south,
        color = "green",  weight = 2) %>%
      addLabelOnlyMarkers(
        data = regions,
        lng = regions$centerLng,
        lat = regions$centerLat,
        label = paste0(as.character(regions$id)),
        labelOptions = labelOptions(
          noHide = T, 
          direction = "top", 
          textOnly = T, 
          style = list('color' = 'green', 'font-size' = '12px')
        )
      )
  })
    
  
  # ==================== INPUT PANEL ====================
  output$timeslots <- renderUI(selectInput(
    "timeslots", 
    label = "", 
    choices = timeslots$label)
  )
  
  output$algorithms <- renderUI(selectInput(
    "algorithms",
    label = "",
    choices = algorithms
  ))
  
  # ==================== DATA MINING ====================
  output$list <- renderDataTable({
    # Handle selected input
    selectedTimeslot <- eventReactive(input$submit, input$timeslots)
    selectedTimeslot <- selectedTimeslot()
    selectedAlgorithm <- eventReactive(input$submit, input$algorithms)
    selectedAlgorithm <- selectedAlgorithm()
    
    # Import data
    withProgress(message = "Importing data ...", value = 1/4, {
      # Read tracks from corresponding CSV file
      timeslot <- timeslots %>% filter(label == selectedTimeslot)
      tracks <- fread(as.character(timeslot$path))
      
      # Build region matrix
      incProgress(1/4, message = "Building region matrix ...")
      Sys.sleep(1)
      nodes <- buildRegionMatrix(tracks)
      
      # Execute algorithm
      incProgress(1/4, message = paste("Executing", selectedAlgorithm, "..."))
      Sys.sleep(1)
      result <- executeRanking(nodes, selectedAlgorithm)
      
      # Render results
      incProgress(1/4, message = "Rendering results ...")
      Sys.sleep(1)
      if (selectedAlgorithm == "PageRank") {
        sortedResult <- data.frame(No = c(1:100), result[order(-result$PageRank),])
        output$chart <- renderPlotly({
          plot_ly(result, x = ~Region, y = ~PageRank)
        })
      } else {
        sortedResult <- data.frame(No = c(1:100), result[order(-result$Average),])
        output$chart <- renderPlotly({
          plot_ly(result, x = ~Region, y = ~Average)
        })
      }
    })
      
    return(sortedResult)
  }, options = list(pageLength = 10))
  
  
  
  # ==============================================================
  # ======================= TAXI BEHAVIOR ========================
  # ==============================================================
  
  # ==================== INPUT PANEL ====================
  output$users <- renderUI(selectInput(
    "users",
    label = "",
    choices = users$id
  ))
  
  output$timeslots2 <- renderUI(selectInput(
    "timeslots2", 
    label = "", 
    choices = timeslots$label)
  )
  
  # ==================== DATA MINING ====================
  output$map2 <- renderLeaflet({
    # Handle selected input
    selectedTimeslot <- eventReactive(input$submit2, input$timeslots2)
    selectedTimeslot <- selectedTimeslot()
    selectedUser <- eventReactive(input$submit2, input$users)
    selectedUser <- selectedUser()
    
    # Import data
    withProgress(message = "Importing data ...", value = 1/4, {
      # Read tracks from corresponding CSV file
      timeslot <- timeslots %>% filter(label == selectedTimeslot)
      tracks <- fread(as.character(timeslot$path))
      trajectories <- filter(tracks, user == selectedUser)
      
      if (nrow(trajectories) > 0) {
        # Removing stop points
        incProgress(1/4, message = "Removing stop points ...")
        Sys.sleep(1)
        lats <- trajectories$lat
        lngs <- trajectories$lng
        for (i in 1:(nrow(trajectories) - 1)) {
          if (lats[i] == lats[i + 1] && lngs[i] == lngs[i + 1]) {
            trajectories <- trajectories[-(i + 1),]
          }
        }
        
        # Execute Traclus
        incProgress(1/4, message = "Executing Traclus ...")
        Sys.sleep(1)
        result <- traclus(trajectories)
        
        # Render result
        incProgress(1/4, message = "Rendering result ...")
        Sys.sleep(1)
        leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
          setView(lng = viewPoint$lng, lat = viewPoint$lat, zoom = 11) %>%
          addTiles() %>%
          addPolylines(
            data = trajectories, 
            lng = trajectories$lng, 
            lat = trajectories$lat, 
            color = "green", weight = 2) %>%
          addPolylines(
            data = result, 
            lng = result$lng, 
            lat = result$lat, 
            color = "red", weight = 2)
      }
    })
  })
  
})