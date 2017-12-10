getTimeslots <- function() {
  timeslots <- c("01:00:00 - 07:30:00", "07:30:00 - 10:30:00", "10:30:00 - 13:00:00")
}

getCSVPath <- function(timeslot, day) {
  if (timeslot == "01:00:00 - 07:30:00") {
    if (day == "Work Day") {
      path <- "data/tracks11.csv"
    } else {
      path <- "data/tracks12.csv"
    }
  } else if (timeslot == "07:30:00 - 10:30:00") {
    if (day == "Work Day") {
      path <- "data/tracks21.csv"
    } else {
      path <- "data/tracks22.csv"
    }
  } else {
    if (day == "Work Day") {
      path <- "data/tracks31.csv"
    } else {
      path <- "data/tracks32.csv"
    }
  }
  return(path)
}

getSeconds <- function(time) {
  tokens <- unlist(strsplit(time, ":"))
  hour <- tokens[1]
  minute <- tokens[2]
  second <- tokens[3]
  seconds <- as.numeric(hour) * 3600 + as.numeric(minute) * 60 + as.numeric(second)
}