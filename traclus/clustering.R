distanceMatrix <- matrix()

UNCLASSIFIED <- -2
NOISE <- -1
GAMMA <- 0

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
  options(digits = 15)
  minEntropy <- 9999
  minEpsilon <- 9999
  minTotalSize <- 9999
  
  for (epsilon in 20:40) {
    sizes <- c()
    for (i in 1:length(lineSegments)) {
      neighborhoods <- getNeighborhoods(i, epsilon)
      sizes <- c(sizes, length(neighborhoods))
    }
    entropy <- 0
    totalSize <- sum(sizes)
    for (i in 1:length(sizes)) {
      probability <- sizes[i] / totalSize
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
  
  print(clusters)
  
  # Generate representative trajectory
  # for (i in 1:length(clusters)) {
  #   generateRepresentativeTrajectory(lineSegments, clusters[[i]])
  # }
  generateRepresentativeTrajectory(lineSegments, clusters[[1]], minLns)
}

generateRepresentativeTrajectory <- function(allLineSegments, cluster, minLns) {
  # Line segments in cluster
  lineSegments <- list()
  for (i in 1:length(cluster)) {
    lineSegments[[i]] <- allLineSegments[[cluster[i]]]
  }
  print(lineSegments)
  
  # Find average direction vector
  sum <- c(0, 0, 0)
  for (i in 1:length(cluster)) {
    start <- lineSegments[[i]][1:3]
    end <- lineSegments[[i]][4:6]
    sum <- sum + (end - start)
  }
  avg <- sum / length(cluster)
  
  # Find theta berween average direction vector and X axis
  theta <- computeAngle(avg, c(1, 0, 0))
  
  # Rotate linesegments so that X axis is parallel to average direction vector
  points <- data.frame()
  for (i in 1:length(lineSegments)) {
    rotatedLs <- rotateLineSegment(lineSegments[[i]], theta)
    points <- rbind(points, rotatedLs[,1])
    points <- rbind(points, rotatedLs[,2])
  }
  colnames(points) <- c('X', 'Y', 'Z')
  
  # Sort the points in the set P by their X'-values
  points <- points[order(points$X),]
  
  # Find representative trajectory
  representativeTrajectory <- list()
  for (i in 1:nrow(points)) {
    point <- points[i,]
    
    numP <- 0
    hittingLineSegments <- list()
    for (j in 1:length(lineSegments)) {
      if (lineSegments[[j]][1] <= point$X && lineSegments[[j]][4] >= point$X) {
        numP <- numP + 1
        hittingLineSegments[[length(hittingLineSegments) + 1]] <- lineSegments[[j]]
      }
    }
    
    if (numP >= 1) {
      if (i > 1) {
        diff <- abs(point$X - points[i - 1,]$X)
      } else {
        diff <- abs(point$X)
      }
      if (diff >= GAMMA) {
        avgPP <- computeAverageCoordinate(point$X, hittingLineSegments)
        avgP <- rotateVector(avgPP, -1 * theta)
        representativeTrajectory[[length(representativeTrajectory) + 1]] <- avgP
      }      
    }
  }
  
  return(representativeTrajectory)
}

computeAverageCoordinate <- function(X, hittingLineSegments) {
  avgPP <- c(0, 0, 0) 
  for (i in 1:length(hittingLineSegments)) {
    avgPP <- avgPP + findIntersectionBetweenPlaneAndLineSegment(
      c(X, 0, 0), c(1, 0, 0), hittingLineSegments[[i]][1:3], hittingLineSegments[[i]][4:6])
  }
  return(avgPP)
}