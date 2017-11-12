source("traclus/input.R")
source("traclus/partitioning.R")
source("traclus/clustering.R")

executeTraclus <- function(tracks) {
  # Partitioning phase
  lineSegments <- c()
  #dates <- getDates(tracks)
  dates <- c("2017-07-21")
  #dates <- c("2017-07-01", "2017-07-02", "2017-07-03", "2017-07-04", "2017-07-05", "2017-07-07", "2017-07-08", "2017-07-09", "2017-07-10", "2017-07-11")
  for (i in 1:length(dates)) {
    trajectory <- getTrajectory(tracks, dates[i])
    lineSegments <- c(lineSegments, partitioning(trajectory))
  }
  print(lineSegments)
  
  # Clustering phase
  generateCluster(lineSegments)
}

