source("ranking/region.R")
regions <- drawMap()
write.table(regions, "data/regions.txt", sep = ",", row.names = FALSE, col.names = FALSE) 