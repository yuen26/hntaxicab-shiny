source("common/geometry.R")

getDestination <- function(source, bearing, distance) {
  distance <- distance / EARTH_RADIUS
  bearing <- toRad(bearing)
  
  lat1 <- toRad(source$lat)
  lng1 <- toRad(source$lng)
  
  lat2 <- asin(sin(lat1) * cos(distance) + cos(lat1) * sin(distance) * cos(bearing))
  lng2 <- lng1 + atan2(sin(bearing) * sin(distance) * cos(lat1), cos(distance) - sin(lat1) * sin(lat2))
  list(lat = toDeg(lat2), lng = toDeg(lng2))
}