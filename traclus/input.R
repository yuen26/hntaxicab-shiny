library(data.table)
library(dplyr)

PATH <- "data/traclus.csv"

writeTrajectories <- function(selectedUser, selectedLabel) {
  source("data/timeslot.R")
  path <- getPath(selectedLabel)
  datTracks <- fread(path)
  tracks <- filter(datTracks, user == selectedUser)
  tracks <- subset(tracks, select = c(date, time, lat, lng))
  fwrite(tracks, PATH)
}

readTrajectories <- function() {
  fread(PATH)
}

getDates <- function(trajectories) {
  unique(trajectories$date)
}

removeStop <- function(trajectory) {
  lats <- trajectory$lat
  lngs <- trajectory$lng
  
  flags <- rep(1, nrow(trajectory))
  for (i in 1:(nrow(trajectory) - 1)) {
    if (flags[i] == 1) {
      if (lats[i] == lats[i + 1] && lngs[i] == lngs[i + 1]) {
        flags[i] <- 0
      }
    }
  }
  trajectory$flag <- flags
  
  subset(trajectory, flag == 1, select = c(lat, lng))
}

getTrajectory <- function(trajectories, selectedDate) {
  trajectory <- filter(trajectories, date == selectedDate)
  removeStop(trajectory)
}