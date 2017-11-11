source("traclus/input.R")
source("traclus/partitioning.R")
source("traclus/estimation.R")
source("traclus/clustering.R")

executeTraclus <- function(trajectories) {
  # Partitioning phase
  lineSegments <- data.frame(start = list(), end = list())
  #dates <- getDates(trajectories)
  dates <- c("2017-07-21")
  #dates <- c("2017-07-01", "2017-07-02", "2017-07-03", "2017-07-04", "2017-07-05", "2017-07-07", "2017-07-08", "2017-07-09", "2017-07-10", "2017-07-11")
  for (i in 1:length(dates)) {
    trajectory <- getTrajectory(trajectories, dates[i])
    lineSegments <- rbind(lineSegments, partitioning(trajectory))
  }
  print(nrow(lineSegments))
  
  # Estimate parameters
  parameters <- estimateParameters(lineSegments)
  epsilon <- parameters[[1]]
  minLns <- parameters[[2]] + 2
  print(paste('epsilon:', epsilon, 'minLns:', minLns))
  
  # Clustering phase
  generateCluster(lineSegments, epsilon, minLns)
}

