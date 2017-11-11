buildRegionMatrix <- function(tracks) {
  # Extract columns
  users <- tracks$user
  dates <- tracks$date
  regions <- tracks$region
  
  # Build adjacency matrix   
  nodes <- matrix(0, nrow = 100, ncol = 100)
  for (i in 1:(nrow(tracks) - 1)) {
    if (regions[i] != 0 && regions[i + 1] != 0) {
      if (users[i] == users[i + 1] && dates[i] == dates[i + 1] && regions[i] != regions[i + 1]) {
        nodes[regions[i], regions[i + 1]] <- nodes[regions[i], regions[i + 1]] + 1
      }
    }
  }
  
  nodes  
}