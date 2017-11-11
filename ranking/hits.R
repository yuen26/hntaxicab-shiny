#' HITS algorithm
#' @param nodes adjacency matrix of nodes
#' @param steps total steps of the loop
#' @return a dataframe of label and average of authority and hub of regions
HITS <- function(nodes, steps) {
  # init
  length <- nrow(nodes)         # number of nodes
  authorities <- rep(1, length) # the authority score of nodes
  hubs <- rep(1, length)        # the hub score of nodes
  
  # start the loop
  for (k in 1:steps) {
    # update authorities
    norm <- 0;
    for (i in 1:length) {
      authorities[i] <- 0
      authorities[i] <- sum(hubs[incomingNeighbors(i, nodes)])
      norm <- norm + (authorities[i]) ^ 2;
    }
    
    # normalize authorities
    norm <- sqrt(norm)
    for (i in 1:length) {
      authorities[i] <- authorities[i] / norm
    }
    
    # update hubs
    norm <- 0;
    for (i in 1:length){
      hubs[i] <- 0
      hubs[i] <- sum(authorities[outcomingNeighbors(i, nodes)]);
      norm <- norm + (hubs[i]) ^ 2;
    }
    
    # normalize hubs
    norm <- sqrt(norm)
    for (i in 1:length) {
      hubs[i] <- hubs[i] / norm
    }
  }
  
  # calculate average value of hub and authority for a node
  # then sort result by this value
  averages <- c()
  for (i in 1:length) {
    averages <- c(averages, (hubs[i] + authorities[i]) / 2)
  }
  
  # result here
  result <- data.frame(Region = 1:100, Hub = hubs, Authority = authorities, Average = averages)
}


#' Find the set of nodes that link to a node X
#' @param index is the index of node X
#' @param nodes is the adjacency matrix of nodes
incomingNeighbors <- function(index, nodes) {   
  which(nodes[,index] > 0)
}

#' Find the set of nodes that node X links to
#' @param index is an index of node X
#' @param nodes is the adjacency matrix of nodes
outcomingNeighbors <- function(index, nodes) {   
  which(nodes[index,] > 0)
}