# Test distance functions
source("traclus/geometry.R")

#si <- c(21.1932019, 105.7805454)
#ei <- c(21.1924745, 105.7806113)
#sj <- c(21.1921392, 105.7800577)
#ej <- c(21.1921828, 105.7786275)

ls1 <- data.frame(start = list(lat = 21.1932019, lng = 105.7805454), end = list(lat = 21.1924745, lng = 105.7806113))
ls2 <- data.frame(start = list(lat = 21.1921392, lng = 105.7800577), end = list(lat = 21.1921828, lng = 105.7786275))

print(measureDistanceBetweenLineSegments(ls1, ls2))