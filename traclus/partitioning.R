# ===================== MDL functions ======================
descriptionCost <- function(trajectory, startIndex, endIndex) {
  startSegment <- trajectory[[startIndex]]
  endSegment <- trajectory[[endIndex]]
  distance <- measureEuclidDistance(startSegment, endSegment)
  if (distance < 1) {
    distance <- 1
  }
  return(log2(distance))
}

encodingCost <- function(trajectory, startIndex, endIndex) {
  startComponent <- trajectory[[startIndex]]
  endComponent <- trajectory[[endIndex]]
  sum <- 0
  for (i in startIndex:(endIndex - 1)) {
    startSegment <- trajectory[[i]]
    endSegment <- trajectory[[i + 1]]
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
  return(sum)
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
  cp <- list()
  cp <- c(cp, list(trajectory[[1]]))
  
  startIndex <- 1
  length <- 1
  
  while (startIndex + length <= length(trajectory)) {
    currIndex <- startIndex + length
    costPar <- MDLPar(trajectory, startIndex, currIndex)
    costNoPar <- MDLNoPar(trajectory, startIndex, currIndex)
    # check if partitioning at the current point makesthe MDL cost larger than not partitioning
    if (costPar > costNoPar) {
      # partition at the previous point
      cp <- c(cp, list(trajectory[[currIndex - 1]]))
      startIndex <- currIndex - 1
      length <- 1
    } else {
      length <- length + 1
    }
  }
  
  cp <- c(cp, list(trajectory[[length(trajectory)]]))
  
  # Remove stop points
  flags <- rep(1, length(cp))
  for (i in 1:(length(cp) - 1)) {
    if (flags[i] == 1) {
      if (cp[[i]][1] == cp[[i + 1]][1] && cp[[i]][2] == cp[[i + 1]][2]) {
        flags[i] <- 0
      }
    }
  }
  newCp <- list()
  for (i in 1:(length(flags))) {
    if (flags[i] == 1) {
      newCp <- c(newCp, list(cp[[i]]))
    }
  }
  
  # Build line segments
  lineSegments <- list()
  for (i in 1:(length(newCp) - 1)) {
    lineSegments[[i]] <- c(newCp[[i]], newCp[[i + 1]])
  }
  return(lineSegments)
}