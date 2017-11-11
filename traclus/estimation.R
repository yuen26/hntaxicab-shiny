distanceMatrix <- matrix()

getNeighborhoods <- function(lineSegments, i, epsilon) {
  lineSegment <- lineSegments[i,]
  neighborhoods <- c()
  for (j in 1:nrow(lineSegments)) {
    if (i == j) {
      distance <- 0
    } else {
      if (distanceMatrix[j, i] != 0) {
        distance <- distanceMatrix[j, i]
      } else {
        distance <- measureDistanceBetweenLineSegments(lineSegment, lineSegments[j,])
        distanceMatrix[i, j] <<- distance
        distanceMatrix[j, i] <<- distance
      }
    }
    if (distance <= epsilon) {
      neighborhoods <- c(neighborhoods, j)
    }
  }
  neighborhoods
}

estimateParameters <- function(lineSegments) {
  minEntropy <- 9999
  minEpsilon <- 9999
  minTotalSize <- 9999
  
  for (epsilon in 20:40) {
    distanceMatrix <<- matrix(0, nrow = nrow(lineSegments), ncol = nrow(lineSegments))
    totalNeighborhoods <- c()
    totalSize <- 0
    for (i in 1:nrow(lineSegments)) {
      neighborhoods <- getNeighborhoods(lineSegments, i, epsilon)
      totalNeighborhoods <- c(totalNeighborhoods, neighborhoods)
      totalSize <- totalSize + length(neighborhoods)
    }
    entropy <- 0
    for (i in 1:length(totalNeighborhoods)) {
      probability <- length(totalNeighborhoods[i]) / totalSize
      entropy <- entropy + probability * log2(probability)
    }
    entropy <- -1 * entropy
    if (entropy < minEntropy) {
      minEntropy <- entropy
      minEpsilon <- epsilon
      minTotalSize <- totalSize
    }
  }
  
  minLns <- ceiling(minTotalSize / nrow(lineSegments))
  list(minEpsilon = minEpsilon, minLns = minLns)
}