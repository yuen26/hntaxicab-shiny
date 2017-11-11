source("common/distance.R")

# Length of region edge is 1 km
REGION_EDGE <- 1

# Number of rows and columns
ROWS <- 10
COLS <- 10

# Center of first region of first row
START_CENTER <- list(lat = 21.05543834, lng = 105.78671064)

# Region dataframe
regions <- data.frame(
  id = numeric(), 
  centerLat = numeric(),
  centerLng = numeric(),
  north = numeric(), 
  south = numeric(), 
  east = numeric(), 
  west = numeric()
)
id <- 0

# Draw a region
drawRegion <- function(centerPoint) {
  distance <- REGION_EDGE / 2
  
  northPoint <- getDestination(centerPoint, 0, distance)
  southPoint <- getDestination(centerPoint, 180, distance)
  eastPoint <- getDestination(centerPoint, 90, distance)
  westPoint <- getDestination(centerPoint, 270, distance)
  
  id <<- id + 1
  regions[nrow(regions) + 1,] <<- c(id, centerPoint$lat, centerPoint$lng, northPoint$lat, southPoint$lat, eastPoint$lng, westPoint$lng)
}

# Draw a row
drawRow <- function(firstRegionCenter) {
  # Draw first region
  drawRegion(firstRegionCenter)
  
  # Draw another region
  for (i in 1:(COLS - 1)) {
    nextRegionCenter <- getDestination(firstRegionCenter, 90, i * REGION_EDGE)
    drawRegion(nextRegionCenter)
  }
}

# Calculate view point
getViewPoint <- function(regions) {
  point <- list(lat = regions[45,]$centerLat, lng = regions[45,]$centerLng)
  getDestination(point, 135, sqrt(REGION_EDGE / 2 * REGION_EDGE / 2 * 2))
}

# Draw map
drawMap <- function() {
  # Draw first row
  drawRow(START_CENTER)
  
  # Draw other rows
  for (i in 1:(ROWS - 1)) {
    nextRowFirstCenter <- getDestination(START_CENTER, 180, i * REGION_EDGE)
    drawRow(nextRowFirstCenter)
  }
  
  regions
}