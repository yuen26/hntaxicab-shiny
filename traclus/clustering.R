library(dplyr)

distanceMatrix <- matrix()

UNCLASSIFIED <- -2
NOISE <- -1

buildDistanceMatrix <- function(lineSegments) {
  distanceMatrix <<- matrix(0, nrow = length(lineSegments), ncol = length(lineSegments))
  for (i in 1:length(lineSegments)) {
    for (j in 1:length(lineSegments)) {
      if (i == j) {
        distance <- 0
      } else {
        if (distanceMatrix[j, i] != 0) {
          distanceMatrix[i, j] <<- distanceMatrix[j, i]
        } else {
          distanceMatrix[i, j] <<- measureDistanceBetweenLineSegments(lineSegments[[i]], lineSegments[[j]])
          distanceMatrix[j, i] <<- distanceMatrix[i, j]
        }
      }
    }
  }
}

getNeighborhoods <- function(i, epsilon) {
  neighborhoods <- c()
  for (j in 1:nrow(distanceMatrix)) {
    if (distanceMatrix[i, j] <= epsilon) {
      neighborhoods <- c(neighborhoods, j)
    }
  }
  return(neighborhoods)
}

estimateParameters <- function(lineSegments) {
  minEntropy <- 9999
  minEpsilon <- 9999
  minTotalSize <- 9999
  
  for (epsilon in 20:40) {
    totalNeighborhoods <- c()
    totalSize <- 0
    for (i in 1:length(lineSegments)) {
      neighborhoods <- getNeighborhoods(i, epsilon)
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
  
  minLns <- ceiling(minTotalSize / length(lineSegments))
  
  return(c(minEpsilon, minLns))
}

generateCluster <- function(lineSegments) {
  # Build distance matrix
  buildDistanceMatrix(lineSegments)
  print(distanceMatrix)
  
  # Estimate parameters
  parameters <- estimateParameters(lineSegments)
  epsilon <- parameters[1]
  minLns <- parameters[2]
  print(paste("eps:", epsilon, "minLns:", minLns))
  
  # STEP 1
  clusters <- list()
  clusterIds <- rep(UNCLASSIFIED, length(lineSegments))
  clusterId <- 1
  
  for (i in 1:length(lineSegments)) {
    if (clusterIds[i] == UNCLASSIFIED) {
      neighborhoods <- getNeighborhoods(i, epsilon)
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
          neighborhoods <- getNeighborhoods(M, epsilon)
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
  
  return(clusters)
}