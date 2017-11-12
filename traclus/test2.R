# Test distance functions
source("traclus/geometry.R")

#si <- c(21.1932019, 105.7805454)
#ei <- c(21.1924745, 105.7806113)
#sj <- c(21.1921392, 105.7800577)
#ej <- c(21.1921828, 105.7786275)

ls1 <- data.frame(start = list(lat = 21.1932019, lng = 105.7805454), end = list(lat = 21.1924745, lng = 105.7806113))
ls2 <- data.frame(start = list(lat = 21.1921392, lng = 105.7800577), end = list(lat = 21.1921828, lng = 105.7786275))
ls3 <- data.frame(start = list(lat = 21.1940565, lng = 105.7775065), end = list(lat = 21.1951459, lng = 105.7778477))
ls4 <- data.frame(start = list(lat = 21.1963673, lng = 105.7782711), end = list(lat = 21.1979676, lng = 105.776818))

print(measureDistanceBetweenLineSegments(ls1, ls2))
print(measureDistanceBetweenLineSegments(ls3, ls4))

vector1 <- c(52.79840, -16.12217, 77.04081)
vector2 <- c(52.79840, -16.12217, 77.04081)
print(all(vector1 == vector2))
print(acos(sum(vector1 * vector2) / (sqrt(sum(vector1 * vector1)) * sqrt(sum(vector2 * vector2)))))