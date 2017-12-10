library(data.table)
library(dplyr)

source("traclus/geometry.R")

getDates <- function(tracks) {
  return(unique(tracks$date))
}

removeStopPoints <- function(tracks) {
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
  return(tracks)
}

getTrajectory <- function(tracks, selectedDay) {
  tracks <- tracks %>% filter(date == selectedDay) %>% select(lat, lng)
  return(removeStopPoints(tracks))
}

getGeoTrajectory <- function(trajectory) {
  geoTrajectory <- list()
  lats <- trajectory$lat
  lngs <- trajectory$lng
  for (i in 1:nrow(trajectory)) {
    geoTrajectory[[i]] <- getGeoVector(lats[i], lngs[i])
  }
  return(geoTrajectory)
}