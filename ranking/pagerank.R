library(igraph)

#' PageRank algorithm
#' 
#' @param nodes adjacency matrix of nodes
#' @return a dataframe of label and pagerank score of regions
PR <- function(nodes) {
  # create adjacency graph from the given matrix rMatrix.  
  g <- graph.adjacency(nodes, mode = "directed", weighted = TRUE)
  
  # call page.rank function from igraph library
  pagerank <- page.rank(g, damping = 0.85, directed = TRUE, weights = NULL)$vector
  
  # result here
  result <- data.frame(Region = 1:100, PageRank = pagerank)
}