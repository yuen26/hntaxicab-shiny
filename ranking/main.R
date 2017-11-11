executeRanking <- function(nodes, algorithm) {
  if (algorithm == "PageRank") {
    source("ranking/pagerank.R")
    result <- PR(nodes)
  } else {
    source("ranking/hits.R")
    result <- HITS(nodes, 5)
  }
}