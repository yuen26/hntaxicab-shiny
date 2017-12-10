# Length of region edge is 1 km
REGION_EDGE <- 1

# Number of rows and columns
ROWS <- 10
COLS <- 10

# Center of first region of first row
START_CENTER <- list(lat = 21.05543725, lng = 105.7674374)

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

# Earth radius (km)
EARTH_RADIUS <- 6371

getRad <- function(deg) {
  return(deg * pi / 180)
}

getDeg <- function(rad) {
  return(rad * 180 / pi)
}

getDestination <- function(source, bearing, distance) {
  distance <- distance / EARTH_RADIUS
  bearing <- getRad(bearing)
  
  lat1 <- getRad(source$lat)
  lng1 <- getRad(source$lng)
  
  lat2 <- asin(sin(lat1) * cos(distance) + cos(lat1) * sin(distance) * cos(bearing))
  lng2 <- lng1 + atan2(sin(bearing) * sin(distance) * cos(lat1), cos(distance) - sin(lat1) * sin(lat2))
  list(lat = getDeg(lat2), lng = getDeg(lng2))
}

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

# Draw map
drawMap <- function() {
  regions <<- regions[0,]
  id <<- 0
  
  # Draw first row
  drawRow(START_CENTER)
  
  # Draw other rows
  for (i in 1:(ROWS - 1)) {
    nextRowFirstCenter <- getDestination(START_CENTER, 180, i * REGION_EDGE)
    drawRow(nextRowFirstCenter)
  }
  
  regions
}

# Calculate view point
getViewPoint <- function(regions) {
  point <- list(lat = regions[45,]$centerLat, lng = regions[45,]$centerLng)
  getDestination(point, 135, sqrt(REGION_EDGE / 2 * REGION_EDGE / 2 * 2))
}