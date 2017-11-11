library(dplyr)

source("common/distance.R")

UNCLASSIFIED <- -2
NOISE <- -1

distanceMatrix <- matrix()

buildDistanceMatrix <- function(lineSegments) {
  distanceMatrix <- matrix(0, nrow = nrow(lineSegments), ncol = nrow(lineSegments))
  for (i in 1:nrow(distanceMatrix)) {
    for (j in 1:ncol(distanceMatrix)) {
      if (i != j && distanceMatrix[i, j] == 0) {
        distanceMatrix[i, j] <- measureDistanceBetweenLineSegments(lineSegments[i,], lineSegments[j,])
        distanceMatrix[j, i] <- distanceMatrix[i, j]
      }      
    }
  }
  print(distanceMatrix)
}

getNeighborhoods <- function(lineSegments, i, epsilon) {
  buildDistanceMatrix(lineSegments)
  
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

generateCluster <- function(lineSegments, epsilon, minLns) {
  clusters <- list()
  
  # STEP 1
  clusterId <- 1
  clusterIds <- rep(UNCLASSIFIED, nrow(lineSegments))
  distanceMatrix <<- matrix(0, nrow = nrow(lineSegments), ncol = nrow(lineSegments))
  
  for (i in 1:nrow(lineSegments)) {
    if (clusterIds[i] == UNCLASSIFIED) {
      neighborhoods <- getNeighborhoods(lineSegments, i, epsilon)
      print(paste(i, 'has neighborhoods:', length(neighborhoods)))
      
      if (length(neighborhoods) >= minLns) {
        cluster <- c()
        
        Q <- c()
        for (j in 1:length(neighborhoods)) {
          clusterIds[neighborhoods[j]] <- clusterId
          cluster <- c(cluster, neighborhoods[j])
          if (neighborhoods[j] != i) {
            Q <- c(Q, neighborhoods[j])
          }
        }
        
        # STEP 2: Expand cluster
        while (length(Q) > 0) {
          M <- Q[1]
          neighborhoods <- getNeighborhoods(lineSegments, M, epsilon)
          if (length(neighborhoods) >= minLns) {
            for (j in 1:length(neighborhoods)) {
              if (clusterIds[neighborhoods[j]] == UNCLASSIFIED || clusterIds[neighborhoods[j]] == NOISE) {
                if (clusterIds[neighborhoods[j]] == UNCLASSIFIED) {
                  Q <- c(Q, neighborhoods[j])
                }
                clusterIds[neighborhoods[j]] <- clusterId
                cluster <- c(cluster, neighborhoods[j])
              }
            }
          }
          if (length(Q) == 1) {
            Q <- c()
          } else {
            Q <- tail(Q, length(Q) - 1) 
          }
        }
        
        clusters[[clusterId]] <- sort(cluster)
        print(paste(i, '-> cluster:', clusterId))
        clusterId <- clusterId + 1
      } else {
        print(paste(i, '-> noise'))
        clusterIds[i] <- NOISE
      }
    }
  }
  
  # STEP 3
  removedClusters <- c()
  if (length(clusters) > 0) {
    for (i in 1:length(clusters)) {
      if (length(clusters[[i]]) < minLns) {
        removedClusters <- c(removedClusters, i)
      }
    }
    if (length(removedClusters) > 0) {
      clusters <- clusters[-removedClusters]
    } 
  }
  
  clusters
}