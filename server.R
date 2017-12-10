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

source("ranking/region.R")
source("ranking/timeslot.R")
source("ranking/matrix.R")
source("ranking/pagerank.R")
source("ranking/hits.R")
source("color.R")

shinyServer(function(input, output) {
  
  heatMapColors <- getHeatMapColors()
  clusterColors <- getClusterColors()
  
  # ==============================================================
  # =================== TRAFFIC FLAW RANKING =====================
  # ==============================================================
  
  # ============================= MAP ============================
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
        color = "green", weight = 2) %>%
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
    choices = getTimeslots()
  ))
  
  output$days <- renderUI(selectInput(
    "days", 
    label = "",
    choices = c("Work Day", "Rest Day")
  ))
  
  output$algorithms <- renderUI(selectInput(
    "algorithms",
    label = "",
    choices = c("PageRank", "HITS")
  ))
  
  
  # ==================== DATA MINING ====================
  output$table <- renderDataTable({
    # Handle selected input
    selectedTimeslot <- eventReactive(input$submit, input$timeslots)
    selectedTimeslot <- selectedTimeslot()
    selectedDay <- eventReactive(input$submit, input$days)
    selectedDay <- selectedDay()
    selectedAlgorithm <- eventReactive(input$submit, input$algorithms)
    selectedAlgorithm <- selectedAlgorithm()
    
    # Import data
    withProgress(message = "Importing data ...", value = 1/4, {
      # Read tracks from corresponding CSV file
      tracks <- fread(getCSVPath(selectedTimeslot, selectedDay))
      
      # Build region matrix
      incProgress(1/4, message = "Building region matrix ...")
      Sys.sleep(1)
      nodes <- buildRegionMatrix(tracks)
      
      # Execute algorithm
      incProgress(1/4, message = paste("Executing", selectedAlgorithm, "..."))
      Sys.sleep(1)
      if (selectedAlgorithm == "PageRank") {
        result <- PR(nodes)
      } else {
        result <- HITS(nodes, 5)
      }
      
      # Render results
      incProgress(1/4, message = "Rendering results ...")
      Sys.sleep(1)
      if (selectedAlgorithm == "PageRank") {
        sortedResult <- data.frame(No = c(1:100), result[order(-result$PageRank),])
        regions$rank <- result$PageRank
      } else {
        sortedResult <- data.frame(No = c(1:100), result[order(-result$Average),])
        regions$rank <- result$Average
      }
      
      regions <- regions[order(-regions$rank),]
      regions$color <- heatMapColors
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
          setView(lng = viewPoint$lng, lat = viewPoint$lat, zoom = 13) %>%
          addTiles() %>%
          addRectangles(
            data = regions,
            lng1 = regions$east, lat1 = regions$north,
            lng2 = regions$west, lat2 = regions$south,
            color = "#333333", weight = 2,
            fillColor = regions$color, fillOpacity = 0.5) %>%
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
    })
      
    return(sortedResult)
  }, options = list(pageLength = 10))
  
  
  
  # ==============================================================
  # ======================= TAXI BEHAVIOR ========================
  # ==============================================================
  tracks <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    }
    read.csv(input$file$datapath)
  })
  
  output$map2 <- renderLeaflet({
    tracks <- tracks()
    if (!is.null(tracks)) {
      source("traclus/input.R")
      source("traclus/partitioning.R")
      source("traclus/clustering.R")
      
      # Get dates (a trajectory is tracks in a date)
      dates <- getDates(tracks)
      
      # Partitioning phase
      print("=================== PARTITIONING PHASE ===================")
      lineSegments <- c()
      for (i in 1:length(dates)) {
        print(dates[i])
        
        # Trajectory
        trajectory <- getTrajectory(tracks, dates[i])
        print("Trajectory removed stop points:")
        print(trajectory)
        
        # Convert cartesian to geographic coordinate
        geoTrajectory <- getGeoTrajectory(trajectory)
        print("Trajectory as geographic coordinate:")
        print(geoTrajectory)
        
        lineSegments <- c(lineSegments, partitioning(geoTrajectory))
      }
      
      print("Line segments:")
      print(lineSegments)
      
      # Clustering phase
      print("=================== GROUPING PHASE ===================")
      clusters <- generateCluster(lineSegments)
      print(clusters)
      
      # Output
      print("=================== OUTPUT ===================")
      viewPoint2 <- tracks[1,]
      map2 <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 20)) %>%
        setView(lng = viewPoint2$lng, lat = viewPoint2$lat, zoom = 15) %>%
        addTiles() %>%
        addPolylines(data = tracks, lat = tracks$lat, lng = tracks$lng, color = "black", weight = 3, opacity = 1)
      
      for (i in 1:length(clusters)) {
        print(paste("--- Cluster", i, " ---"))
        cluster <- clusters[[i]]
        lats <- c()
        lngs <- c()
        for (j in 1:length(cluster)) {
          lineSegment <- lineSegments[[cluster[j]]]
          start <- getCartesianVector(lineSegment[1], lineSegment[2], lineSegment[3])
          end <- getCartesianVector(lineSegment[4], lineSegment[5], lineSegment[6])
          lats <- c(lats, start[1], end[1])
          lngs <- c(lngs, start[2], end[2])
          print(paste(start[1], start[2], end[1], end[2]))
        }
        points <- data.frame(lat = lats, lng = lngs)
        map2 <- addPolylines(map2, data = points, lat = points$lat, lng = points$lng, color = clusterColors[i], weight = 3, opacity = 1)
      }
      
      map2
    }
  })
})