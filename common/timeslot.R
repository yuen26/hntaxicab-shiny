LABELS <- c("00:00:00 - 07:30:00", "07:30:00 - 10:30:00", "10:30:00 - 16:00:00")
PATHS <- c("data/tracks1.csv", "data/tracks2.csv", "data/tracks3.csv")

getTimeslots <- function() {
  timeslots <- data.frame(LABELS, PATHS)
  colnames(timeslots) <- c("label", "path")
  timeslots
}

getPath <- function(label) {
  for (i in (1:length(LABELS))) {
    if (LABELS[i] == label) {
      return(PATHS[i])
    }
  }
}

getSeconds <- function(time) {
  tokens <- unlist(strsplit(time, ":"))
  hour <- tokens[1]
  minute <- tokens[2]
  second <- tokens[3]
  seconds <- as.numeric(hour) * 3600 + as.numeric(minute) * 60 + as.numeric(second)
}