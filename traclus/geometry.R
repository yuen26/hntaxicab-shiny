options(digits = 15)

# Earth radius (meter)
EARTH_RADIUS <- 6371 * 1000

getRad <- function(deg) {
  return(deg * pi / 180)
}

getDeg <- function(rad) {
  return(rad * 180 / pi)
}

#=============== VECTOR FUNCTIONS ===============

get3DVector <- function(lat, lng) {
  lat <- getRad(lat)
  lng <- getRad(lng)
  vector <- c()
  vector[1] <- EARTH_RADIUS * cos(lat) * cos(lng)
  vector[2] <- EARTH_RADIUS * cos(lat) * sin(lng)
  vector[3] <- EARTH_RADIUS * sin(lat)
  return(vector)
}

computeAngle <- function(vector1, vector2) {
  if (vector1[1] == vector2[1] && vector1[2] == vector2[2] && vector1[3] == vector2[3]) {
    return(0)
  }
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

rotateVector <- function(vector, angle) {
  rotationMatrix <- matrix(c(1, 0, 0, 0, cos(angle), -1 * sin(angle), 0, sin(angle), cos(angle)), ncol = 3)
  return(rotationMatrix %*% vector)
}

rotateLineSegment <- function(lineSegment, angle) {
  lineSegmentMatrix <- matrix(lineSegment, nrow = 3) 
  rotationMatrix <- matrix(c(1, 0, 0, 0, cos(angle), -1 * sin(angle), 0, sin(angle), cos(angle)), ncol = 3)
  return(rotationMatrix %*% lineSegmentMatrix)
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
  leni <- measureEuclidDistance(si, ei)
  lenj <- measureEuclidDistance(sj, ej)
  
  if (leni < lenj) {
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
  leni <- measureEuclidDistance(si, ei)
  lenj <- measureEuclidDistance(sj, ej)
  
  if (leni < lenj) {
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
  leni <- measureEuclidDistance(si, ei)
  lenj <- measureEuclidDistance(sj, ej)
  
  if (leni == 0 || lenj == 0) {
    return(0)
  }
  
  if (leni < lenj) {
    shortLength <- leni 
    vector1 <- ej - sj
    vector2 <- ei - si
  } else {
    shortLength <- lenj
    vector1 <- ei - si
    vector2 <- ej - sj
  }
  
  theta <- computeAngle(vector1, vector2)
  return(shortLength * sin(theta))
}

measureDistanceBetweenLineSegments <- function(lsi, lsj) {
  si <- lsi[1:3]
  ei <- lsi[4:6]
  sj <- lsj[1:3]
  ej <- lsj[4:6]
  return(measurePerpendicularDistance(si, ei, sj, ej) 
  + measureParallelDistance(si, ei, sj, ej) 
  + measureAngleDistance(si, ei, sj, ej))
}

findIntersectionBetweenPlaneAndLineSegment <- function(rootPoint, planeNormal, startPoint, endPoint) {
  d <- measureEuclidDistance(rootPoint, startPoint)
  u <- startPoint - endPoint
  t <- -1 * d / sum(u * planeNormal)
  if (0 <= t && t <= 1) {
    return(startPoint + t * u)
  } else {
    return(NULL)
  }
}