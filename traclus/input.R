library(data.table)
library(dplyr)

source("traclus/geometry.R")

getTrajectory <- function(tracks, selectedDay) {
  # Select tracks by date
  tracks <- tracks %>% filter(date == selectedDay) %>% select(lat, lng)
  
  # Remove stop points
  lats <- tracks$lat
  lngs <- tracks$lng
  flags <- rep(1, nrow(tracks))
  for (i in 1:(nrow(tracks) - 1)) {
    if (flags[i] == 1) {
      if (lats[i] == lats[i + 1] && lngs[i] == lngs[i + 1]) {
        flags[i] <- 0
      }
    }
  }
  tracks$flag <- flags
  tracks <- tracks %>% filter(flag == 1)
  
  # Convert points to vector 3D
  trajectory <- list()
  lats <- tracks$lat
  lngs <- tracks$lng
  for (i in 1:nrow(tracks)) {
    trajectory[[i]] <- get3DVector(lats[i], lngs[i])
  }
  return(trajectory)
}

# writeTrajectories <- function(selectedUser, selectedLabel) {
#   source("data/timeslot.R")
#   path <- getPath(selectedLabel)
#   datTracks <- fread(path)
#   tracks <- filter(datTracks, user == selectedUser)
#   tracks <- subset(tracks, select = c(date, time, lat, lng))
#   fwrite(tracks, "data/traclus.csv")
# }