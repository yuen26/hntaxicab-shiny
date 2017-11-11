source("traclus/input.R")
source("common/distance.R")

# ===================== MDL functions ======================
descriptionCost <- function(trajectory, startIndex, endIndex) {
  startSegment <- trajectory[startIndex,]
  endSegment <- trajectory[endIndex,]
  
  distance <- measureEuclidDistance(startSegment, endSegment)
  if (distance < 1) {
    distance <- 1
  }
  
  log2(distance)
}

encodingCost <- function(trajectory, startIndex, endIndex) {
  startComponent <- trajectory[startIndex,]
  endComponent <- trajectory[endIndex,]
  
  if (startComponent$lat == endComponent$lat && startComponent$lng == endComponent$lng) {
    return(0)
  }
  
  sum <- 0
  for (i in startIndex:(endIndex - 1)) {
    startSegment <- trajectory[i,]
    endSegment <- trajectory[i + 1,]
    
    perpendicularDistance <- measurePerpendicularDistance(startComponent, endComponent, startSegment, endSegment)
    angleDistance <- measureAngleDistance(startComponent, endComponent, startSegment, endSegment)
    if (perpendicularDistance < 1) {
      perpendicularDistance <- 1
    } 
    if (angleDistance < 1) {
      angleDistance <- 1
    }   
    
    sum <- sum + log2(perpendicularDistance) + log2(angleDistance)
  }
  sum
}

MDLPar <- function(trajectory, startIndex, endIndex) {
  descriptionCost(trajectory, startIndex, endIndex) + encodingCost(trajectory, startIndex, endIndex)
}

MDLNoPar <- function(trajectory, startIndex, endIndex) {
  descriptionCost(trajectory, startIndex, endIndex)
}

# =========== Approximate Trajectory Partitioning ============
partitioning <- function(trajectory) {
  # characteristic points
  cp <- trajectory[1,]
  
  startIndex <- 1
  length <- 1
  
  while (startIndex + length <= nrow(trajectory)) {
    currIndex <- startIndex + length
    costPar <- MDLPar(trajectory, startIndex, currIndex)
    costNoPar <- MDLNoPar(trajectory, startIndex, currIndex)
    # check if partitioning at the current point makesthe MDL cost larger than not partitioning
    if (costPar > costNoPar) {
      # partition at the previous point
      cp <- rbind(cp, trajectory[currIndex - 1,])
      startIndex <- currIndex - 1
      length <- 1
    } else {
      length <- length + 1
    }
  }
  
  cp <- rbind(cp, trajectory[nrow(trajectory),])
  
  # Remove stop point
  cp <- removeStop(cp)
  
  # Build line segments
  lineSegments <- data.frame(start = list(), end = list())
  for (i in 1:(nrow(cp) - 1)) {
    lineSegment <- data.frame(start = cp[i,], end = cp[i + 1,])
    lineSegments <- rbind(lineSegments, lineSegment)
  }
  lineSegments
}