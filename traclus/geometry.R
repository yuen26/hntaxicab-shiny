# Earth radius (meter)
EARTH_RADIUS <- 6371 * 1000

getRad <- function(deg) {
  return(deg * pi / 180)
}

getDeg <- function(rad) {
  return(rad * 180 / pi)
}

#=============== VECTOR FUNCTIONS ===============

getVector <- function(point) {
  #point = c(lat, lng)
  lat <- getRad(point[1])
  lng <- getRad(point[2])
  vector <- c()
  vector[1] <- EARTH_RADIUS * cos(lat) * cos(lng)
  vector[2] <- EARTH_RADIUS * cos(lat) * sin(lng)
  vector[3] <- EARTH_RADIUS * sin(lat)
  return(vector)
}

computeAngle <- function(vector1, vector2) {
  # theta is radian
  theta <- acos(sum(vector1 * vector2) / (sqrt(sum(vector1 * vector1)) * sqrt(sum(vector2 * vector2))))
  return(theta)
}

getProjectionOfPointToLine <- function(start, end, point) {
  vector1 <- point - start
  vector2 <- end - start
  coefficient <- sum(vector1 * vector2) / sum(vector2 * vector2)
  return(start + coefficient * vector2)
}


#=============== DISTANCE FUNCTIONS ===============

measureEuclidDistance <- function(source, destination) {
  sqrt((source[1] - destination[1]) ^ 2 + (source[2] - destination[2]) ^ 2 + (source[3] - destination[3]) ^ 2)
  #as.numeric(formatC(distance, digits = 8, format = "f"))
}

measureDistanceFromPointToLine <- function(start, end, point) {
  projection <- getProjectionOfPointToLine(start, end, point)
  measureEuclidDistance(point, projection)
}

measurePerpendicularDistance <- function(si, ei, sj, ej) {
  iLength <- measureEuclidDistance(si, ei)
  jLength <- measureEuclidDistance(sj, ej)
  
  if (iLength < jLength) {
    l1 <- measureDistanceFromPointToLine(sj, ej, si)
    l2 <- measureDistanceFromPointToLine(sj, ej, ei)
  } else {
    l1 <- measureDistanceFromPointToLine(si, ei, sj)
    l2 <- measureDistanceFromPointToLine(si, ei, ej)
  }
  
  if (l1 == 0 && l2 == 0) {
    return(0)
  } else {
    return((l1 ^ 2 + l2 ^ 2) / (l1 + l2))
  }
}

measureParallelDistance <- function(si, ei, sj, ej) {
  iLength <- measureEuclidDistance(si, ei)
  jLength <- measureEuclidDistance(sj, ej)
  
  if (iLength < jLength) {
    projection1 <- getProjectionOfPointToLine(sj, ej, si)
    projection2 <- getProjectionOfPointToLine(sj, ej, ei)
    return(min(measureEuclidDistance(projection1, sj), measureEuclidDistance(projection1, ej), 
        measureEuclidDistance(projection2, sj), measureEuclidDistance(projection2, ej)))
  } else {
    projection1 <- getProjectionOfPointToLine(si, ei, sj)
    projection2 <- getProjectionOfPointToLine(si, ei, ej)
    return(min(measureEuclidDistance(projection1, si), measureEuclidDistance(projection1, ei), 
        measureEuclidDistance(projection2, si), measureEuclidDistance(projection2, ei)))
  }
}

measureAngleDistance <- function(si, ei, sj, ej) {
  iLength <- measureEuclidDistance(si, ei)
  jLength <- measureEuclidDistance(sj, ej)
  
  if (iLength < jLength) {
    shortLength <- iLength 
    vector1 <- ej - sj
    vector2 <- ei - si
  } else {
    shortLength <- jLength
    vector1 <- ei - si
    vector2 <- ej - sj
  }
  
  theta <- computeAngle(vector1, vector2)
  return(shortLength * sin(theta))
}

measureDistanceBetweenLineSegments <- function(lsi, lsj) {
  si <- getVector(c(lsi$start.lat, lsi$start.lng))
  ei <- getVector(c(lsi$end.lat, lsi$end.lng))
  sj <- getVector(c(lsj$start.lat, lsj$start.lng))
  ej <- getVector(c(lsj$end.lat, lsj$end.lng))
  return(measurePerpendicularDistance(si, ei, sj, ej) 
  + measureParallelDistance(si, ei, sj, ej) 
  + measureAngleDistance(si, ei, sj, ej))
}