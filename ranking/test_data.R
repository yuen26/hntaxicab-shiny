library(data.table)
library(dplyr)

workdays <- c("2017-07-03", "2017-07-04", "2017-07-05", "2017-07-06", "2017-07-07",
              "2017-07-10", "2017-07-11", "2017-07-12", "2017-07-13", "2017-07-14",
              "2017-07-17", "2017-07-18", "2017-07-19", "2017-07-20", "2017-07-21",
              "2017-07-24", "2017-07-25", "2017-07-26", "2017-07-27", "2017-07-28",
              "2017-07-31")

restdays <- c("2017-07-01", "2017-07-02",
              "2017-07-08", "2017-07-09",
              "2017-07-15", "2017-07-16",
              "2017-07-22", "2017-07-23",
              "2017-07-29", "2017-07-30")

tracks1 <- fread("data/tracks1.csv")
tracks11 <- tracks1 %>% filter(date %in% workdays)
tracks12 <- tracks1 %>% filter(date %in% restdays)
fwrite(tracks11, "data/tracks11.csv")
fwrite(tracks12, "data/tracks12.csv")

tracks2 <- fread("data/tracks2.csv")
tracks21 <- tracks2 %>% filter(date %in% workdays)
tracks22 <- tracks2 %>% filter(date %in% restdays)
fwrite(tracks21, "data/tracks21.csv")
fwrite(tracks22, "data/tracks22.csv")

tracks3 <- fread("data/tracks3.csv")
tracks31 <- tracks3 %>% filter(date %in% workdays)
tracks32 <- tracks3 %>% filter(date %in% restdays)
fwrite(tracks31, "data/tracks31.csv")
fwrite(tracks32, "data/tracks32.csv")