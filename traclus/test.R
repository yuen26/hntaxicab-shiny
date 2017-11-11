writeTest <- function(trajectory) {
  text <- c()
  for (i in 1:nrow(trajectory)) {
    str <- paste("{lat:", trajectory[i,]$lat, ", lng:", trajectory[i,]$lng, "},")
    text <- c(text, str)
  }
  write(text, "d:/test.txt", sep="\n") 
}

source("traclus/input.R")
#testUser <- "5951c4bfda3e65689ab864ab"
#testLabel <- "00:00:00 - 07:30:00"
#writeTrajectories(testUser, testLabel)
trajectories <- readTrajectories()

source("traclus/main.R")
executeTraclus(trajectories)
