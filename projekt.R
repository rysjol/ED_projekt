library(ggplot2)
library(dplyr)

circuits <- read.csv("2014/circuits_2014.csv")
constructorResults <- read.csv("2014/constructorResults_2014.csv")
constructors <- read.csv("2014/constructors_2014.csv")
constructorStandings <- read.csv("2014/constructorStandings_2014.csv")
drivers <- read.csv("2014/drivers_2014.csv")
driverStandings <- read.csv("2014/driverStandings_2014.csv")
lapTimes <- read.csv("2014/lapTimes_2014.csv")
pitStops <- read.csv("2014/pitStops_2014.csv")
qualifying <- read.csv("2014/qualifying_2014.csv")
races <- read.csv("2014/races_2014.csv")
results <- read.csv("2014/results_2014.csv")
seasons <- read.csv("2014/seasons_2014.csv")
status <- read.csv("2014/status_2014.csv")

drv <- "Fernando Alonso"
drv_split <- unlist(strsplit(drv, " "))
drv_id <- drivers %>% subset(surname == drv_split[2])
drv_id <- drv_id$driverId

drv_results <- results %>% subset(driverId  == drv_id) %>% select(raceId, points)
ggplot(drv_results, aes(x = raceId, y=points)) + geom_point(color="blue") + theme_bw()
